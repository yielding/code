# UIE Proto - 클라이언트 개발 가이드

UIE(Unified Inference Engine) gRPC API 정의 파일입니다.
이 폴더의 `.proto` 파일만으로 모든 언어의 클라이언트를 생성할 수 있습니다.

## 파일 구조

```
proto/
├── common.proto             # 공통 타입 (JobMetadata, BoundingBox, FileChunk 등)
├── operations.proto         # 비동기 작업 관리 (상태 조회, 취소, Admission)
│
├── yolo.proto               # 객체 탐지 (YOLOv5/v8/v10)
├── face.proto               # 얼굴 분석 (검출 + 랜드마크 + 특징 + 속성)
├── whisper.proto            # 음성 인식 (STT)
├── ocr.proto                # 광학 문자 인식
├── number_plate.proto       # 번호판 (검출 + OCR + 복원)
├── classification.proto     # 이미지 분류
├── object_analysis.proto    # 통합 객체 분석 (탐지 → 사람/차량 분기 분석)
├── messenger.proto          # 메신저 앱 탐지 (스크린샷)
├── video.proto              # 비디오 특징 추출
├── nmt.proto                # 기계 번역
├── captioning.proto         # 이미지 캡셔닝
├── search.proto             # 이미지 검색 (CLIP 임베딩)
├── human_parsing.proto      # 인체 세그멘테이션 + 색상 추출
└── image_processing.proto   # 이미지 변환 (초해상도/저조도/디블러)
```

**의존 관계**: 모든 서비스 proto는 `common.proto`와 `operations.proto`를 import합니다.

## 서버 접속 정보

| 프로토콜 | 기본 포트 | 용도 |
|----------|-----------|------|
| gRPC | `50051` | 모든 추론 서비스 |
| HTTP | `5000` | 헬스체크, 메트릭, 프로파일러 UI |

- TLS는 선택적이며 서버 설정에 따라 다름
- gRPC 최대 메시지 크기: **100MB**

## 핵심 호출 패턴: 2단계 비동기

모든 서비스가 동일한 비동기 패턴을 따릅니다.

```
[1단계] Submit (client streaming) → OperationHandle (operation_id)
[2단계] GetResult (operation_id)  → 결과 (완료까지 대기)
```

### 1단계: 파일 업로드 (Submit)

`FileChunk` 스트림으로 파일을 전송합니다.

```
첫 번째 청크:  FileChunk { metadata: JobMetadata { ... } }
중간 청크들:   FileChunk { data: <바이트> }
마지막 청크:   FileChunk { data: <바이트>, is_last: true }
```

**JobMetadata 필수 필드:**

| 필드 | 설명 | 예시 |
|------|------|------|
| `job_id` | 고유 작업 ID (클라이언트 생성) | `"550e8400-e29b-41d4-a716-446655440000"` |
| `task_type` | 작업 유형 | `"detection"`, `"face"`, `"whisper"` |
| `content_type` | MIME 타입 | `"image/jpeg"`, `"audio/wav"` |
| `total_size` | 파일 전체 크기 (바이트) | `1048576` |

**JobMetadata 선택 필드:**

| 필드 | 설명 | 예시 |
|------|------|------|
| `options` | 모델/서비스별 추가 옵션 (map) | 아래 옵션 표 참조 |
| `model_search_paths` | 모델 파일 검색 경로 | `["D:/models", "/opt/models"]` |

### 2단계: 결과 조회 (GetResult)

```protobuf
message OperationResultRequest {
  string operation_id = 1;     // Submit에서 받은 operation_id
  int64 wait_timeout_ms = 2;   // 대기 타임아웃 (밀리초, 0=즉시 반환)
}
```

- `wait_timeout_ms > 0`: 서버가 결과 완료까지 대기 후 반환 (Long Polling)
- `wait_timeout_ms = 0`: 즉시 반환 (아직 미완료면 에러)

## 서비스별 RPC 목록

### 공통 서비스

| 서비스 | RPC | 설명 |
|--------|-----|------|
| `OperationsService` | `GetOperation(OperationIdRequest)` | 작업 상태 조회 |
| | `CancelOperation(OperationIdRequest)` | 작업 취소 |
| | `CheckAdmission(AdmissionRequest)` | 서버 수용 가능 여부 확인 |

### 비전 (이미지)

| 서비스 | Submit RPC | Result RPC | 결과 타입 |
|--------|-----------|------------|-----------|
| `YoloService` | `SubmitDetection` | `GetDetectionResult` | `DetectionResult` |
| `FaceService` | `SubmitFaceAnalysis` | `GetFaceAnalysisResult` | `AnalysisResult` |
| `OcrService` | `SubmitOcr` | `GetOcrResult` | `OcrResult` |
| `ClassificationService` | `SubmitClassification` | `GetClassificationResult` | `ClassificationResult` |
| `ObjectAnalysisService` | `SubmitFrameAnalysis` | `GetFrameAnalysisResult` | `FrameAnalysisResult` |
| `PersonAnalysisService` | `SubmitPersonAnalysis` | `GetPersonAnalysisResult` | `PersonAnalysisResult` |
| `VehicleClassificationService` | `SubmitVehicleClassification` | `GetVehicleClassificationResult` | `ClassificationResult` |
| `MessengerService` | `SubmitDetection` | `GetDetectionResult` | `MessengerDetectionResult` |
| `ImageSearchService` | `SubmitImageSearch` | `GetImageSearchResult` | `ImageSearchResult` |
| `CaptioningService` | `SubmitCaption` | `GetCaptionResult` | `CaptionResult` |
| `ImageProcessingService` | `SubmitTransform` | `GetTransformResult` | `TransformResult` |
| `HumanParsingService` | `SubmitSegmentation` | `GetSegmentationResult` | `SegmentationResult` |
| | `SubmitColorExtraction` | `GetColorExtractionResult` | `ColorExtractionResult` |

### 번호판

| 서비스 | Submit RPC | Result RPC | 결과 타입 |
|--------|-----------|------------|-----------|
| `NumberPlateService` | `SubmitDetection` | `GetDetectionResult` | `NpDetectionResult` |
| | `SubmitOcr` | `GetOcrResult` | `NpOcrResult` |
| | `SubmitRestoration` | `GetRestorationResult` | `NpRestorationResult` |

### 음성/텍스트

| 서비스 | Submit RPC | Result RPC | 결과 타입 |
|--------|-----------|------------|-----------|
| `WhisperService` | `SubmitTranscription` | `GetTranscriptionResult` | `TranscriptionResult` |
| `NmtService` | `SubmitTranslation` | `GetTranslationResult` | `TranslationResult` |

### 비디오

| 서비스 | Submit RPC | Result RPC | 결과 타입 |
|--------|-----------|------------|-----------|
| `VideoFeatureService` | `SubmitVideoFeature` | `GetVideoFeatureResult` | `VideoFeatureResult` |

## options 맵 주요 키

`JobMetadata.options`에 설정할 수 있는 서비스별 옵션입니다.

### YOLO (객체 탐지)

| 키 | 값 | 기본값 | 설명 |
|----|-----|--------|------|
| `model_id` | `yolov8n`, `yolov8s`, `yolov8m`, `yolov8l`, `yolov8x` | `yolov8n` | 모델 선택 |
| `confidence_threshold` | `0.0~1.0` | `0.25` | 신뢰도 임계값 |
| `iou_threshold` | `0.0~1.0` | `0.45` | NMS IoU 임계값 |
| `backend` | `auto`, `safetensors`, `onnx`, `mdm` | `auto` | 모델 백엔드 |
| `execution_mode` | `scheduled`, `direct` | `scheduled` | 실행 모드 |

### Face (얼굴 분석)

| 키 | 값 | 설명 |
|----|-----|------|
| `detection_model` | `ultraface-320`, `ultraface-640`, `retinaface-resnet50-640` 등 | 검출 모델 |
| `landmark_model` | `pfld-68`, `coordreg-106` | 랜드마크 모델 |
| `feature_model` | `arcface-resnet50`, `arcface-mobilefacenet` | 특징 추출 모델 |

### Whisper (음성 인식)

| 키 | 값 | 설명 |
|----|-----|------|
| `model_id` | `whisper-tiny`, `whisper-base`, `whisper-small`, `whisper-medium`, `whisper-large` | 모델 선택 |
| `language` | `ko`, `en`, `ja` 등 (빈 문자열=자동) | 언어 코드 |
| `task` | `transcribe`, `translate` | 작업 유형 |

### Image Processing (이미지 변환)

| 키 | 값 | 설명 |
|----|-----|------|
| `task_type` | `super_resolution`, `low_light`, `deblur` | 변환 유형 |

## 작업 상태 (OperationState)

```
QUEUED(1) → RUNNING(2) → SUCCEEDED(3)
                       → FAILED(4)
                       → CANCELLED(5)
```

## Admission Handshake (권장)

대용량 파일 업로드 전 서버 과부하를 사전 확인하여 대역폭 낭비를 방지합니다.

```
Client → CheckAdmission(model_id) → Server
Server → AdmissionResponse { accepted, retry_after_ms, queue_size }

accepted=true  → Submit 진행
accepted=false → retry_after_ms만큼 대기 후 재시도
```

## 코드 생성

### Python

```bash
pip install grpcio-tools

python -m grpc_tools.protoc \
  -I./proto \
  --python_out=./generated \
  --grpc_python_out=./generated \
  proto/*.proto
```

### C# (.NET)

`.csproj`에 추가:

```xml
<ItemGroup>
  <PackageReference Include="Grpc.Net.Client" Version="2.60.0" />
  <PackageReference Include="Google.Protobuf" Version="3.25.1" />
  <PackageReference Include="Grpc.Tools" Version="2.60.0" PrivateAssets="All" />
</ItemGroup>
<ItemGroup>
  <Protobuf Include="proto\*.proto" GrpcServices="Client" ProtoRoot="proto" />
</ItemGroup>
```

### Go

```bash
go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest

protoc -I./proto \
  --go_out=./generated --go_opt=paths=source_relative \
  --go-grpc_out=./generated --go-grpc_opt=paths=source_relative \
  proto/*.proto
```

### Java / Kotlin (Gradle)

```groovy
plugins {
    id 'com.google.protobuf' version '0.9.4'
}
dependencies {
    implementation 'io.grpc:grpc-netty-shaded:1.60.0'
    implementation 'io.grpc:grpc-protobuf:1.60.0'
    implementation 'io.grpc:grpc-stub:1.60.0'
}
protobuf {
    protoc { artifact = "com.google.protobuf:protoc:3.25.1" }
    plugins { grpc { artifact = "io.grpc:protoc-gen-grpc-java:1.60.0" } }
    generateProtoTasks { all()*.plugins { grpc {} } }
}
```

### TypeScript / Node.js

```bash
npm install @grpc/grpc-js @grpc/proto-loader
# 또는 정적 생성:
npm install grpc_tools_node_protoc_ts
```

## 클라이언트 예제 (Python)

```python
import grpc
import uuid

# 생성된 stub import
from generated import common_pb2, operations_pb2, yolo_pb2, yolo_pb2_grpc

def upload_chunks(file_path: str, task_type: str, content_type: str):
    """FileChunk 스트림 생성기"""
    import os
    file_size = os.path.getsize(file_path)

    # 첫 번째 청크: 메타데이터
    metadata = common_pb2.JobMetadata(
        job_id=str(uuid.uuid4()),
        task_type=task_type,
        content_type=content_type,
        total_size=file_size,
        options={"confidence_threshold": "0.5"},
    )
    yield common_pb2.FileChunk(metadata=metadata)

    # 파일 데이터 청크 (64KB 단위)
    CHUNK_SIZE = 65536
    with open(file_path, "rb") as f:
        while True:
            data = f.read(CHUNK_SIZE)
            if not data:
                break
            is_last = len(data) < CHUNK_SIZE or f.read(1) == b""
            if not is_last:
                f.seek(-1, 1)
            yield common_pb2.FileChunk(data=data, is_last=is_last)

def detect_objects(server_addr: str, image_path: str):
    """YOLO 객체 탐지 예제"""
    channel = grpc.insecure_channel(server_addr)
    stub = yolo_pb2_grpc.YoloServiceStub(channel)

    # 1단계: Submit (스트리밍 업로드)
    handle = stub.SubmitDetection(
        upload_chunks(image_path, "detection", "image/jpeg")
    )
    print(f"Operation ID: {handle.operation_id}")

    # 2단계: GetResult (완료 대기)
    result = stub.GetDetectionResult(
        operations_pb2.OperationResultRequest(
            operation_id=handle.operation_id,
            wait_timeout_ms=30000,  # 30초 대기
        )
    )

    if result.success:
        for det in result.detections:
            print(f"  {det.class_name}: {det.confidence:.2f} "
                  f"[{det.bbox.x_min:.3f}, {det.bbox.y_min:.3f}, "
                  f"{det.bbox.x_max:.3f}, {det.bbox.y_max:.3f}]")
    else:
        print(f"Error: {result.error.code} - {result.error.message}")

# 사용
detect_objects("localhost:50051", "test.jpg")
```

## 클라이언트 예제 (C#)

```csharp
using Grpc.Net.Client;
using Uie.Common;
using Uie.Operations;
using Uie.Yolo;

var channel = GrpcChannel.ForAddress("http://localhost:50051");
var client = new YoloService.YoloServiceClient(channel);

// 1단계: Submit
using var call = client.SubmitDetection();

// 메타데이터 전송
var fileBytes = File.ReadAllBytes("test.jpg");
await call.RequestStream.WriteAsync(new FileChunk
{
    Metadata = new JobMetadata
    {
        JobId = Guid.NewGuid().ToString(),
        TaskType = "detection",
        ContentType = "image/jpeg",
        TotalSize = fileBytes.Length,
    }
});

// 파일 데이터 전송 (64KB 청크)
const int chunkSize = 65536;
for (int offset = 0; offset < fileBytes.Length; offset += chunkSize)
{
    int size = Math.Min(chunkSize, fileBytes.Length - offset);
    await call.RequestStream.WriteAsync(new FileChunk
    {
        Data = Google.Protobuf.ByteString.CopyFrom(fileBytes, offset, size),
        IsLast = offset + size >= fileBytes.Length,
    });
}
await call.RequestStream.CompleteAsync();

var handle = await call.ResponseAsync;
Console.WriteLine($"Operation ID: {handle.OperationId}");

// 2단계: GetResult
var result = await client.GetDetectionResultAsync(new OperationResultRequest
{
    OperationId = handle.OperationId,
    WaitTimeoutMs = 30000,
});

if (result.Success)
{
    foreach (var det in result.Detections)
    {
        Console.WriteLine($"  {det.ClassName}: {det.Confidence:F2} " +
            $"[{det.Bbox.XMin:F3}, {det.Bbox.YMin:F3}, " +
            $"{det.Bbox.XMax:F3}, {det.Bbox.YMax:F3}]");
    }
}
```

## 클라이언트 예제 (C++)

```cpp
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <grpcpp/grpcpp.h>

#include "generated/cpp/common.pb.h"
#include "generated/cpp/operations.pb.h"
#include "generated/cpp/object_analysis.grpc.pb.h"

// UUID 간이 생성 (실제 프로젝트에서는 boost::uuids 등 사용 권장)
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

// 파일을 읽어 바이트 벡터로 반환
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
    auto channel = grpc::CreateCustomChannel(
        server_addr, grpc::InsecureChannelCredentials(), args);
    auto stub = uie::object_analysis::ObjectAnalysisService::NewStub(channel);

    // 파일 읽기
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

    if (result.success()) {
        std::cout << "Detected " << result.objects_size() << " objects:\n";
        for (const auto& obj : result.objects()) {
            const auto& bb = obj.bbox();
            std::printf("  %s: %.2f [%.3f, %.3f, %.3f, %.3f]\n",
                obj.class_name().c_str(), obj.confidence(),
                bb.x_min(), bb.y_min(), bb.x_max(), bb.y_max());

            if (obj.has_person()) {
                std::cout << "    -> Person: "
                          << obj.person().attributes_size() << " attributes, "
                          << obj.person().reid_features_size() << "d ReID\n";
            } else if (obj.has_vehicle()) {
                std::cout << "    -> Vehicle: " << obj.vehicle().vehicle_type()
                          << " (" << obj.vehicle().vehicle_confidence() << ")\n";
            }
        }
    } else {
        std::cerr << "Error: " << result.error().code()
                  << " - " << result.error().message() << "\n";
    }
}

int main(int argc, char* argv[]) 
{
    std::string image = (argc > 1) ? argv[1] : "test.jpg";
    analyze_frame("localhost:50051", image);
    return 0;
}
```

**빌드 (CMake):**

```cmake
cmake_minimum_required(VERSION 3.16)
project(uie_example)

find_package(Protobuf REQUIRED)
find_package(gRPC REQUIRED)

add_library(uie_proto
  generated/cpp/common.pb.cc       generated/cpp/common.grpc.pb.cc
  generated/cpp/operations.pb.cc   generated/cpp/operations.grpc.pb.cc
  generated/cpp/object_analysis.pb.cc generated/cpp/object_analysis.grpc.pb.cc
)
target_link_libraries(uie_proto PUBLIC gRPC::grpc++ protobuf::libprotobuf)

add_executable(frame_analysis_example example.cpp)
target_link_libraries(frame_analysis_example PRIVATE uie_proto)
```

## 에러 처리

### gRPC 상태 코드

| 코드 | 의미 | 대응 |
|------|------|------|
| `OK` (0) | 성공 | - |
| `INVALID_ARGUMENT` (3) | 잘못된 요청 | 요청 파라미터 확인 |
| `NOT_FOUND` (5) | 작업 ID 없음 | operation_id 확인 |
| `RESOURCE_EXHAUSTED` (8) | 서버 과부하 / 큐 가득 참 | `CheckAdmission` 후 재시도 |
| `CANCELLED` (1) | 작업 취소됨 | - |
| `DEADLINE_EXCEEDED` (4) | 타임아웃 | `wait_timeout_ms` 늘리거나 재조회 |
| `INTERNAL` (13) | 서버 내부 오류 | 로그 확인 |

### 결과 레벨 에러

모든 결과 메시지에 `success` (bool)와 `error` (ErrorDetail) 필드가 있습니다.

```protobuf
message ErrorDetail {
  string code = 1;                  // 에러 코드 (예: "MODEL_NOT_FOUND")
  string message = 2;               // 사람이 읽을 수 있는 에러 메시지
  map<string, string> details = 3;  // 추가 상세 정보
}
```

## 모델 백엔드 (ModelBackend)

| 값 | 설명 |
|----|------|
| `MODEL_BACKEND_AUTO` (0) | 서버가 자동 선택 (기본값) |
| `MODEL_BACKEND_SAFETENSORS` (1) | SafeTensors (Candle) |
| `MODEL_BACKEND_ONNX` (2) | ONNX Runtime |
| `MODEL_BACKEND_MDM` (3) | 암호화 ONNX (MDM) |

## 실행 모드 (ExecutionMode)

| 값 | 설명 |
|----|------|
| `EXECUTION_MODE_SCHEDULED` (0) | 스케줄러 경유 (기본값, 큐 관리) |
| `EXECUTION_MODE_DIRECT` (1) | 직접 실행 (스케줄러 우회, 테스트용) |

## 헬스체크

서버 상태 확인에는 두 가지 방법이 있습니다.

### gRPC (common.proto)

```protobuf
// common.proto에 메시지만 정의됨
// 실제 헬스체크는 HTTP API 사용 권장
message HealthCheckRequest {}
message HealthCheckResponse {
  bool healthy = 1;
  string version = 2;
  map<string, string> components = 3;
  GpuStatus gpu = 4;
}
```

### HTTP (권장)

```
GET http://<host>:5000/health

응답:
{
  "healthy": true,
  "version": "0.1.0",
  "gpu": { "available": true, "device_name": "NVIDIA RTX 4090", ... }
}
```

## 좌표계

- **바운딩 박스** (`BoundingBox`): 정규화 좌표 `0.0~1.0` (원본 이미지 기준)
  - 실제 픽셀 좌표 = 정규화 좌표 x 이미지 크기
- **랜드마크** (`Point`): 정규화 좌표 `0.0~1.0`
- **색상** (`ColorResult`): OpenCV HSV 범위 (H: 0~180, S: 0~255, V: 0~255)

## 청크 전송 권장 사항

| 항목 | 권장값 |
|------|--------|
| 청크 크기 | 64KB (65,536 bytes) |
| 최대 파일 크기 | 100MB (gRPC 메시지 제한) |
| 타임아웃 | 모델별 상이 (이미지: 30초, 음성: 120초, 비디오: 300초) |
