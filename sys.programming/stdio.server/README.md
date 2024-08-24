A stdio server typically refers to a server that communicates through standard input (stdin) and standard output (stdout). 
This is often used in the context of language servers or tools that interact with a client (like an editor or IDE) via command-line interfaces.

stdio server는 두 개 이상의 프로세스가 std in / out으로 통신하는 방식
파이프는 그걸 이어주는 도구. 즉 프로그래밍 api에서 stdio server를 만드는데 파이프를 사용한다고 이상하게 생각할 필요가 없다.
결국 파이프는 stdio를 연결해 주는 것이니

## Key Characteristics of a StdIO Server:
1. Communication: 
    It uses standard input and output streams for communication. The client sends requests through stdin, and the server responds through stdout.

2. Simplicity: 
    This model simplifies the setup, as it can run in environments where traditional network sockets are not available.

3. Integration: 
    StdIO servers are commonly used in language servers that follow the Language Server Protocol (LSP), allowing them to work seamlessly with various editors.

4. Process Management: 
    The server runs as a separate process, and the client manages its lifecycle, often spawning it as needed.

## Use Cases:
 - Language Servers: 
    Many language servers, including those for Ruby (like Solargraph) and others, can operate as stdio servers.

- Development Tools: 
   Tools that require real-time interaction with editors or other command-line utilities.


## Example Workflow:
 - The editor starts the stdio server as a subprocess.

 - The editor sends requests (e.g., for code completion) to the server via stdin.

 - The server processes the requests and sends responses back via stdout.

 - The editor receives the responses and updates the user interface accordingly.

 - Using stdio servers allows for flexible and efficient communication between tools and editors, 
   making it a popular choice in modern development environments.

## Usage

python stdio_server.py | python client.py
