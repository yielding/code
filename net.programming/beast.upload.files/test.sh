#!/bin/bash

# Test script for upload client
echo "Testing upload client with test server..."
echo "Server: http://172.16.253.34:8000/infer"
echo ""

# Check if executable exists
if [ ! -f "./build/main" ]; then
    echo "Error: ./build/main not found. Please build the project first."
    echo "Run: make"
    exit 1
fi

# Test with sample images (you'll need to provide actual image files)
./build/main 172.16.253.34 8000 ./1.jpg ./2.jpg