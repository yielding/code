#!/usr/bin/env node

function greeter(person: string) {
  return "Hello, " + person;
}

let user: string = "Lee Chang Ha";
let msg = greeter(user);

console.log(msg);
