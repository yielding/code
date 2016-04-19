#!/usr/bin/env swift

import Foundation

protocol Renderer {
  func moveTo(p: CGPoint)
  func lineTo(p: CGPoint)
  func circleAt(center: CGPoint, radius: CGFloat)
  func arcAt(center: CGPoint, radius: CGFloat, sa: CGFloat, ea: CGFloat)
}

// 1. circleAt과 rectangleAt의 기본 구현을 추가할 수 있다
// 2. rectangleAt은 protocol 내에 requirement가 없기 때문에 나중에
//    customize 되지 않는다.
extension Renderer {
  func circleAt(center: CGPoint, radius: CGFloat) {
    print("circleAt@renderer.ext")
  }

  func rectangleAt(edges: CGRect) {
    print("rectangleAt@renderer.ext")
  }
}

struct TestRenderer : Renderer {
  func moveTo(p: CGPoint) {
    print("moveTo(\(p.x), \(p.y))")
  }

  func lineTo(p: CGPoint) {
    print("lineTo(\(p.x), \(p.y))")
  }

  func arcAt(center: CGPoint, radius: CGFloat, sa: CGFloat, ea: CGFloat) {
    print("arcAt(\(center), radius\(radius))")
  }
}

extension TestRenderer { 
  func circleAt(center: CGPoint, radius: CGFloat) { 
    print("circle@TestRenderer") 
  }

  func rectangleAt(edges: CGRect) {
    print("rectangleAt@TestRenderer")
  }
}


// 1. circleAt의 경우 protocol:8에 circleAt의 requirement가 있다
// 2. requirement는 customization point를 만든다.
// 3. rectangleAt은 protocol의 requirement가 아니다.
// 4. 그래서 Renderer의 extension에 의해 가려진다.(shadow)
let r0: Renderer = TestRenderer()
r0.circleAt(center: CGPoint(x: 0, y: 1), radius: 10)
r0.rectangleAt(edges : CGRect(x:0, y:0, width: 10, height: 20))

let r1 = TestRenderer()
r1.circleAt(center: CGPoint(x: 0, y: 1), radius: 10)
r1.rectangleAt(edges : CGRect(x:0, y:0, width: 10, height: 20))
