#!/usr/bin/env swift

struct Vector2D {
    var x = 0.0, y = 0.0
}

func + (l: Vector2D, r: Vector2D) -> Vector2D {
    return Vector2D(x: l.x + r.x, y: l.y + r.y)
}

func += (inout l: Vector2D, r: Vector2D) {
    l = l + r
}

prefix func - (v: Vector2D) -> Vector2D {
    return Vector2D(x: -v.x, y: -v.y)
}

prefix func ++ (inout v: Vector2D) -> Vector2D {
    v += Vector2D(x: 1.0, y: 1.0)
    return v
}

func - (l: Vector2D, r: Vector2D) -> Vector2D {
    return Vector2D(x: l.x - r.x, y: l.y - r.y)
}

func == (l: Vector2D, r: Vector2D) -> Bool {
    return (l.x == r.x) && (l.y == r.y)
}

func != (l: Vector2D, r: Vector2D) -> Bool {
    return !(l == r)
}

let v0 = Vector2D(x: 3.0, y:1.0)
var v1 = Vector2D(x: 3.0, y:1.0)
let v2 = v0 + v1
print(v2)
print(-v2)

v1 += v0
print(v1)

++v1
print(v1)

var v3 = v1
print(v1 == v3)
print(v1 != v3)
