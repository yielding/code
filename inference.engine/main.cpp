#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <grpcpp/grpcpp.h>

#include "common.pb.h"
#include "operations.pb.h"
#include "object_analysis.grpc.pb.h"

#include <random>
#include <sstream>
#include <iomanip>

auto generate_uuid() -> std::string
{
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<uint32_t> dist(0, 0xFFFFFFFF);

    std::ostringstream ss;
    ss << std::hex << std::setfill('0');
    ss << std::setw(8) << dist(gen) << "-";
    ss << std::setw(4) << (dist(gen) & 0xFFFF) << "-";
    ss << std::setw(4) << ((dist(gen) & 0x0FFF) | 0x4000) << "-";
    ss << std::setw(4) << ((dist(gen) & 0x3FFF) | 0x8000) << "-";
    ss << std::setw(8) << dist(gen) << std::setw(4) << (dist(gen) & 0xFFFF);
    return ss.str();
}

auto read_file(const std::string& path) -> std::vector<char>
{
    std::ifstream ifs(path, std::ios::binary | std::ios::ate);
    if (!ifs) throw std::runtime_error("Cannot open file: " + path);

    auto size = ifs.tellg();
    ifs.seekg(0);
    std::vector<char> buf(size);
    ifs.read(buf.data(), size);
    return buf;
}

void analyze_frame(const std::string& server_addr, const std::string& image_path)
{
    // 채널 생성 (최대 수신 메시지 100MB)
    grpc::ChannelArguments args;
    args.SetMaxReceiveMessageSize(100 * 1024 * 1024);
    auto channel = grpc::CreateCustomChannel(server_addr, grpc::InsecureChannelCredentials(), args);
    auto stub = uie::object_analysis::ObjectAnalysisService::NewStub(channel);

    auto file_data = read_file(image_path);

    // ── 1단계: Submit (client streaming) ──
    grpc::ClientContext submit_ctx;
    uie::operations::OperationHandle handle;
    auto writer = stub->SubmitFrameAnalysis(&submit_ctx, &handle);

    // 첫 번째 청크: 메타데이터
    uie::common::FileChunk meta_chunk;
    auto* meta = meta_chunk.mutable_metadata();
    meta->set_job_id(generate_uuid());
    meta->set_task_type("frame_analysis");
    meta->set_content_type("image/jpeg");
    meta->set_total_size(file_data.size());
    (*meta->mutable_options())["confidence_threshold"] = "0.5";
    writer->Write(meta_chunk);

    // 파일 데이터 청크 (64KB 단위)
    constexpr size_t kChunkSize = 65536;
    for (size_t offset = 0; offset < file_data.size(); offset += kChunkSize)
    {
        size_t len = std::min(kChunkSize, file_data.size() - offset);
        bool is_last = (offset + len >= file_data.size());

        uie::common::FileChunk data_chunk;
        data_chunk.set_data(file_data.data() + offset, len);
        data_chunk.set_is_last(is_last);
        writer->Write(data_chunk);
    }
    writer->WritesDone();

    auto submit_status = writer->Finish();
    if (!submit_status.ok()) {
        std::cerr << "Submit failed: " << submit_status.error_message() << "\n";
        return;
    }
    std::cout << "Operation ID: " << handle.operation_id() << "\n";

    // ── 2단계: GetResult (완료 대기) ──
    grpc::ClientContext result_ctx;
    uie::operations::OperationResultRequest req;
    req.set_operation_id(handle.operation_id());
    req.set_wait_timeout_ms(30000);  // 30초 대기

    uie::object_analysis::FrameAnalysisResult result;
    auto result_status = stub->GetFrameAnalysisResult(&result_ctx, req, &result);
    if (!result_status.ok()) {
        std::cerr << "GetResult failed: " << result_status.error_message() << "\n";
        return;
    }

    if (result.success())
    {
        std::cout << "Detected " << result.objects_size() << " objects:\n";
        for (const auto& obj : result.objects()) 
        {
            const auto& bb = obj.bbox();
            std::printf("  %s: %.2f [%.3f, %.3f, %.3f, %.3f]\n",
                obj.class_name().c_str(), obj.confidence(),
                bb.x_min(), bb.y_min(), bb.x_max(), bb.y_max());

            if (obj.has_person()) 
            {
                const auto& p = obj.person();
                std::cout << "    -> Person: "
                          << p.attributes_size() << " attributes, "
                          << p.reid_features_size() << "d ReID\n";
                for (const auto& color : p.upper_body_colors()) {
                    std::cout << "       upper: " << color.main_name()
                              << " (" << color.branch_name() << ")\n";
                }
                for (const auto& color : p.lower_body_colors()) {
                    std::cout << "       lower: " << color.main_name()
                              << " (" << color.branch_name() << ")\n";
                }
                for (const auto& attr : p.attributes()) {
                    if (attr.detected()) {
                        std::printf("       [attr] %s (%.2f)\n",
                            attr.name().c_str(), attr.confidence());
                    }
                }
            } 
            else if (obj.has_vehicle()) 
            {
                const auto& v = obj.vehicle();
                std::printf("    -> Vehicle: %s (%.2f)\n",
                    v.vehicle_type().c_str(), v.vehicle_confidence());
                for (const auto& prob : v.probabilities()) {
                    std::printf("       %s: %.3f\n",
                        prob.class_name().c_str(), prob.probability());
                }
            }
        }

        if (result.has_metadata()) 
        {
            const auto& m = result.metadata();
            std::printf("\nProcessing: %.1fms, model=%s, device=%s\n",
                m.processing_time_ms(),
                m.model_name().c_str(),
                m.device().c_str());
        }
    } 
    else 
    {
        std::cerr << "Error: " << result.error().code()
                  << " - " << result.error().message() << "\n";
    }
}

int main(int argc, char* argv[])
{
    std::string image = (argc > 1) ? argv[1] : "test.jpg";
    std::string server = (argc > 2) ? argv[2] : "localhost:50051";
    analyze_frame(server, image);
    return 0;
}
