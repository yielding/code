# YOLOv8 Object Detector (C++ / OpenCV DNN + FFmpeg)

YOLOv8 ONNX 모델과 OpenCV DNN 모듈을 사용한 C++ 객체 탐지기.
이미지 또는 동영상에서 객체를 탐지하고, 바운딩 박스가 그려진 결과와 메타데이터를 출력한다.

## 요구 사항

- CMake 3.30+
- C++26 지원 컴파일러 (Apple Clang 21+)
- OpenCV 4.x (DNN 모듈 포함)
- FFmpeg 8.x (libavformat, libavcodec, libavutil, libswscale)
- Python 3 + ultralytics (모델 export용)

### macOS 설치

```bash
brew install opencv ffmpeg
```

## 프로젝트 구조

```
├── main.cpp              # CLI 진입점 (이미지/동영상 자동 판별)
├── detection.hpp         # Detection 구조체, COCO 클래스, 바운딩 박스 그리기, 메타데이터 출력
├── yolo_detector.hpp     # YOLODetector 클래스 (OpenCV DNN 기반 추론)
├── video_processor.hpp   # VideoProcessor 클래스 (FFmpeg 기반 디코딩/인코딩)
├── CMakeLists.txt        # 빌드 설정
└── download_model.sh     # 모델 다운로드 스크립트
```

## 모델 준비

YOLOv8 ONNX 모델은 `.pt`에서 export해야 한다.

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install ultralytics onnx
```

```python
from ultralytics import YOLO
model = YOLO("yolov8n.pt")       # 자동 다운로드
model.export(format="onnx", opset=12, imgsz=640)
```

또는 CLI로:

```bash
yolo export model=yolov8n.pt imgsz=640 format=onnx opset=12
```

모델 크기별 선택:

| 모델 | 파일 크기 | 정확도 | 속도 |
|------|----------|--------|------|
| yolov8n | ~12MB | 낮음 | 빠름 |
| yolov8s | ~23MB | | |
| yolov8m | ~52MB | | |
| yolov8l | ~87MB | | |
| yolov8x | ~131MB | 높음 | 느림 |

## 빌드

```bash
cmake -B build
cmake --build build
```

## 사용법

```
./build/yolo_detect <image|video> <model.onnx> [options]

Options:
  --classes <c1,c2,...>  탐지할 클래스 필터 (예: person,car)
  --conf <0.0-1.0>      확률 임계값 (기본값: 0.5)
  --output <path>       출력 경로 (기본값: output/)
  --json <path>         이미지 모드: 메타데이터를 JSON으로 저장
```

입력 파일의 확장자로 이미지/동영상을 자동 판별한다.
- 이미지: `.jpg`, `.png`, `.bmp` 등
- 동영상: `.mp4`, `.avi`, `.mov`, `.mkv`, `.webm` 등

### 이미지 모드

```bash
# 기본 실행 → output/photo_detected.jpg 생성
./build/yolo_detect photo.jpg yolov8n.onnx

# 특정 클래스만 탐지
./build/yolo_detect photo.jpg yolov8n.onnx --classes person,car

# 낮은 임계값 + JSON 메타데이터
./build/yolo_detect photo.jpg yolov8n.onnx --conf 0.25 --json result.json
```

### 동영상 모드

```bash
# 기본 실행 → output/video_detected.mp4 + output/video_detected.json 생성
./build/yolo_detect video.mp4 yolov8n.onnx

# 특정 클래스 + 출력 디렉토리 지정
./build/yolo_detect video.mp4 yolov8n.onnx --classes person --output results/

# 낮은 임계값
./build/yolo_detect video.mp4 yolov8n.onnx --conf 0.3
```

동영상 모드는 자동으로:
- `{output_dir}/{파일명}_detected.mp4` — 바운딩 박스가 그려진 동영상
- `{output_dir}/{파일명}_detected.json` — 프레임별 탐지 메타데이터

를 생성한다.

### 출력 예시

콘솔 (이미지 모드):
```
Loading model: yolov8n.onnx
Mode: image (850x479)
Running inference (conf >= 50%)...
+---------------------------------------------------------+
|  Detection Results                                      |
+------+--------------+--------+-------------------------+
|  #   | Class        | Conf.  | Bounding Box (x,y,w,h)  |
+------+--------------+--------+-------------------------+
|    1 | person       |  59.8% | (  70,  57, 777, 365)   |
+------+--------------+--------+-------------------------+
Total: 1 object(s) detected
Output image saved to: output/1_detected.jpg
```

콘솔 (동영상 모드):
```
Loading model: yolov8n.onnx
Mode: video
Video: 1920x1080, 30.00 fps, 900 frames
Processing: 900/900 frames (100%)
Output video saved to: output/video_detected.mp4
Metadata saved to: output/video_detected.json
```

동영상 메타데이터 JSON:
```json
{
  "source": "video.mp4",
  "width": 1920,
  "height": 1080,
  "fps": 30,
  "total_frames": 900,
  "frames": [
    { "frame": 0, "detections": [
      { "class": "person", "confidence": 0.85, "box": [100, 200, 50, 120] }
    ]},
    { "frame": 1, "detections": [] }
  ]
}
```

## 지원 클래스 (COCO 80)

person, bicycle, car, motorcycle, airplane, bus, train, truck, boat,
traffic light, fire hydrant, stop sign, parking meter, bench, bird, cat,
dog, horse, sheep, cow, elephant, bear, zebra, giraffe, backpack, umbrella,
handbag, tie, suitcase, frisbee, skis, snowboard, sports ball, kite,
baseball bat, baseball glove, skateboard, surfboard, tennis racket, bottle,
wine glass, cup, fork, knife, spoon, bowl, banana, apple, sandwich, orange,
broccoli, carrot, hot dog, pizza, donut, cake, chair, couch, potted plant,
bed, dining table, toilet, tv, laptop, mouse, remote, keyboard, cell phone,
microwave, oven, toaster, sink, refrigerator, book, clock, vase, scissors,
teddy bear, hair drier, toothbrush
