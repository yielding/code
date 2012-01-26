#!/usr/bin/env node

// these modules need to be imported in order to use them.
// Node has several modules.  They are like any #include
// or import statement in other languages
var http = require("http");
var url = require("url");

// The most important line in any Node file.  This function
// does the actual process of creating the server.  Technically,
// Node tells the underlying operating system that whenever a
// connection is made, this particular callback function should be
// executed.  Since we're creating a web service with REST API,
// we want an HTTP server, which requires the http variable
// we created in the lines above.
// Finally, you can see that the callback method receives a 'request'
// and 'response' object automatically.  This should be familiar
// to any PHP or Java programmer.
http.createServer(function(request, response) {
  // The response needs to handle all the headers, and the return codes
  // These types of things are handled automatically in server programs
  // like Apache and Tomcat, but Node requires everything to be done yourself
  response.writeHead(200, {"Content-Type": "text/plain"});

  // Here is some unique-looking code.  This is how Node retrives
  // parameters passed in from client requests.  The url module
  // handles all these functions.  The parse function
  // deconstructs the URL, and places the query key-values in the
  // query object.  We can find the value for the "number" key
  // by referencing it directly - the beauty of JavaScript.
  var params = url.parse(request.url, true).query;
  var input = params.number;

  // These are the generic JavaScript methods that will create
  // our random number that gets passed back to the caller
  var numInput = new Number(input);
  var numOutput = new Number(Math.random() * numInput).toFixed(0);

  // Write the random number to response
  response.write(numOutput);

  // Node requires us to explicitly end this connection.  This is because
  // Node allows you to keep a connection open and pass data back and forth,
  // though that advanced topic isn't discussed in this article.
  response.end();

  // When we create the server, we have to explicitly connect the HTTP server to
  // a port.  Standard HTTP port is 80, so we'll connect it to that one.
}).listen(1234);

// Output a String to the console once the server starts up, letting us know everything
// starts up correctly
console.log("Random Number Generator Running...");
