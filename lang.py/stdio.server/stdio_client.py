#!/usr/bin/env python

import sys
import json

def send_request(data):
    # Send a request to the server
    print(json.dumps(data))
    sys.stdout.flush()
    # Read the response from the server
    response = sys.stdin.readline()
    return json.loads(response)

if __name__ == "__main__":
    # Example request
    request = {"message": "Hello, Server!"}
    response = send_request(request)
    print("Server response:", response)
