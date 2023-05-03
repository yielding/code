#!/usr/bin/env node

class User {
  constructor(name, age) {
    this.name = name;
    this.age = age;
  }

  showName() { console.log(this.name); }
  showAge() { console.log(this.age); }
}

const yielding = new User("yielding", 50);
yielding.showName();
