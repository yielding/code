#!/usr/bin/env python

import sys
import json

def process_request(request):
    # Example processing: Echoing back the received request
    return {"response": f"Received: {request}"}

def main():
    for line in sys.stdin:
        # Read the request from stdin
        request = json.loads(line.strip())
        # Process the request
        response = process_request(request)
        # Write the response to stdout
        print(json.dumps(response))
        sys.stdout.flush()  # Ensure the output is flushed

if __name__ == "__main__":
    main()
