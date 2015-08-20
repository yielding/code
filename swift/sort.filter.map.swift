#!/usr/bin/env swift

let x = [10, 30, 20, 0]
    .sort   { $0 < $1 }
    .filter { $0 > 10 }
    .map    { $0 * 10 }

print(x)
