#!/usr/bin/env python3

import subprocess

# 서버 실행
proc = subprocess.Popen(
    ["./build/main"],  # C++로 빌드된 실행 파일
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,  # <-- stderr도 캡처
    text=True  # 문자열 입출력
)

# 보낼 메시지 (LSP 스타일)
json_message = '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}'
content_length = len(json_message)
message = f"Content-Length: {content_length}\r\n\r\n{json_message}"

# 메시지 전송
proc.stdin.write(message)
proc.stdin.flush()

# 응답 수신
response_header = proc.stdout.readline()
response_body = proc.stdout.read(content_length)  # 혹은 .read() 전체 읽기

print("✅ [stdout] Response header:", response_header)
print("✅ [stdout] Response body  :", response_body)

# stderr 출력도 수신
stderr_output = proc.stderr.read()
print("⚠️ [stderr]:", stderr_output)