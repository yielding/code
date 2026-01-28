#!/bin/bash
################################################################################
#
# Generate Ruby code from proto file
#
################################################################################

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT_DIR="${SCRIPT_DIR}/ruby_gen"
GRPC_PLUGIN="/opt/homebrew/bin/grpc_ruby_plugin"

mkdir -p "${OUTPUT_DIR}"

protoc \
  --ruby_out="${OUTPUT_DIR}" \
  --grpc_out="${OUTPUT_DIR}" \
  --plugin=protoc-gen-grpc="${GRPC_PLUGIN}" \
  -I "${SCRIPT_DIR}" \
  "${SCRIPT_DIR}/image_processor.proto"

echo "Generated Ruby files in ${OUTPUT_DIR}:"
ls -la "${OUTPUT_DIR}"
