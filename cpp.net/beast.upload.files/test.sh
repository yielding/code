#!/bin/bash

# Test script for upload client
echo "Testing upload client with test server..."
echo "Server: http://172.16.253.34:8000/infer"
echo ""

# Check if executable exists
if [ ! -f "./main" ]; then
    echo "Error: ./main not found. Please build the project first."
    echo "Run: make"
    exit 1
fi

# Test with sample images (you'll need to provide actual image files)
./build/main 172.16.253.34 8000 /Users/yielding/Desktop/1.jpg /Users/yielding/Desktop/2.jpg

# Alternative test without images (will use defaults)
# ./main 172.16.253.34 8000