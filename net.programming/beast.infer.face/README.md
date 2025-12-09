# TritonClient

Boost.Beast 기반 추론 서버 클라이언트 라이브러리

## 빌드

```bash
mkdir -p build && cd build
cmake ..
make
```

## 사용법

### 기본 사용

```cpp
#include "triton_client.hpp"
#include <nlohmann/json.hpp>

using json = nlohmann::json;

int main()
{
  net::TritonClient client("172.16.253.34", "8004");

  // Face 추론
  auto face_models = json{
    {"detect", "Retina640"},
    {"landmark", "CoordReg"},
    {"feature", "ArcFace"}
  };

  auto result = client.infer("Face", face_models, {"face1.png", "face2.jpg"});

  if (result)
    println("Result: {}", result->body);
  else
    println(stderr, "Error: {}", result.error());

  return 0;
}
```

### 다양한 분석 타입

```cpp
// OCR 추론
auto ocr_models = json{
  {"detect", "CRAFT"},
  {"ocr", "CRNN"}
};
auto result = client.infer("OCR", ocr_models, {"document.png"});

// License Plate 추론
auto lpr_models = json{
  {"detect", "YOLOv8"},
  {"ocr", "LPRNet"}
};
auto result = client.infer("LPR", lpr_models, {"car.jpg"});
```

### 메모리에서 직접 전송

```cpp
// 파일을 메모리로 읽은 후 전송
vector<unsigned char> image_data = load_image_from_memory();

auto result = client.infer(
  "Face",
  face_models,
  "image.png",      // 파일명 (MIME 타입 결정용)
  image_data        // 이미지 바이너리 데이터
);
```

### Progress 콜백

```cpp
auto progress = [](size_t sent, size_t total) {
  println("Progress: {}/{} bytes", sent, total);
};

auto result = client.infer("Face", face_models, images, "/infer", progress);
```

### 커스텀 엔드포인트

```cpp
auto result = client.infer("Face", face_models, images, "/v2/infer");
```

## API

### 생성자

```cpp
TritonClient(string host, string port);
```

### infer (파일 경로)

```cpp
auto infer(
  string_view analyzer_type,
  const json& model_types,
  const vector<string>& image_paths,
  string_view endpoint = "/infer",
  const ProgressHandler& progress_handler = nullptr
) -> expected<Response, string>;
```

### infer (메모리)

```cpp
auto infer(
  string_view analyzer_type,
  const json& model_types,
  string_view filename,
  const vector<unsigned char>& content,
  string_view endpoint = "/infer",
  const ProgressHandler& progress_handler = nullptr
) -> expected<Response, string>;
```

### Response 구조체

```cpp
struct Response
{
  unsigned status_code;  // HTTP 상태 코드
  string body;           // 응답 본문 (JSON)
  bool success;          // 성공 여부
};
```

### 연결 관리

```cpp
auto is_connected() const -> bool;
auto reconnect_if_needed() -> void;
auto disconnect() -> void;
```

## 설정

| 상수 | 값 | 설명 |
|------|-----|------|
| `MAX_FILE_SIZE` | 100MB | 최대 파일 크기 |
| `DEFAULT_TIMEOUT` | 30초 | 요청 타임아웃 |

## 의존성

- C++26
- Boost.Beast
- Boost.Asio
- Boost.UUID
- nlohmann/json
