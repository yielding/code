//implementing a stack, create a object
function Stack() {
  this.list = [];
}

Stack.prototype.push = function(value) {
  this.list.push(value);
}

Stack.prototype.pop = function() {
  return this.list.pop();
}

Stack.prototype.size = function() {
  return this.list.length;
}

//create a custom toString function,
//event though Array object has its own
//we want to customize it a little bit
Stack.prototype.toString = function() {
  var temp = '[ ';
  for (var i = 0, length = this.list.length; i < length; i++) {
    temp += this.list[i];
    if (length - i != 1) {
      temp += ', ';
    }
  }
  temp += ' ]';
  return temp;
}

var stack = new Stack();
stack.push(1);
stack.push(2);
stack.push(3);
stack.push(4);
stack.push(5);
//alert(stack.toString());
console.log(stack.toString());
