#!/bin/bash
# Download YOLOv8n ONNX model from Ultralytics GitHub releases
# Usage: ./download_model.sh [model_name]
# model_name: yolov8n, yolov8s, yolov8m, yolov8l, yolov8x (default: yolov8n)

MODEL=${1:-yolov8n}
URL="https://github.com/ultralytics/assets/releases/download/v8.3.0/${MODEL}.onnx"

echo "Downloading ${MODEL}.onnx..."
curl -L -o "${MODEL}.onnx" "$URL"
echo "Done: ${MODEL}.onnx"
