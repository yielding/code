#include <arrow/api.h>
#include <arrow/io/api.h>
#include <arrow/ipc/api.h>
#include <iostream>

using arrow::Status;
using arrow::Result;
using arrow::io::ReadableFile;
using arrow::ipc::RecordBatchFileReader;

int main() 
{
  // 1. Arrow 파일 열기
  auto infile_result = ReadableFile::Open("example.arrow");
  if (!infile_result.ok()) 
  {
    cerr << "파일 열기 실패: " << infile_result.status().ToString() << "\n";
    return 1;
  }

  shared_ptr<arrow::io::ReadableFile> infile = infile_result.ValueOrDie();

  // 2. FileReader 생성
  auto reader_result = RecordBatchFileReader::Open(infile);
  if (!reader_result.ok()) 
  {
    cerr << "FileReader 생성 실패: " << reader_result.status().ToString() << "\n";
    return 1;
  }

  shared_ptr<RecordBatchFileReader> reader = reader_result.ValueOrDie();

  // 3. 레코드 배치 순회
  for (int i = 0; i < reader->num_record_batches(); ++i) 
  {
    auto batch_result = reader->ReadRecordBatch(i);
    if (!batch_result.ok()) 
    {
      cerr << "배치 읽기 실패: " << batch_result.status().ToString() << "\n";
      continue;
    }
    auto batch = batch_result.ValueOrDie();

    cout << "Batch " << i << ": " << batch->num_rows() << " rows\n";
    for (int col = 0; col < batch->num_columns(); ++col)
      cout << "  Column: " << batch->column_name(col) << "\n";
  }

  return 0;
}
