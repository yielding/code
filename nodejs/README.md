## Things to know

1. HTTP is a first class citizen in Node, designed with streaming and low latency in mind. This makes Node well suited for the foundation of a web library or framework.
2. Just because Node is designed without threads, doesn't mean you cannot take advantage of multiple cores in your environment. Child processes can be spawned by using our child_process.fork() API, and are designed to be easy to communicate with.
3. Built upon that same interface is the cluster module, which allows you to share sockets between processes to enable load balancing over your cores.
