import Foundation

let queue = DispatchQueue.global()

queue.async {
  var result = 0
  for i in 1..<100 {
    result += i
  }

  result += 10

  var main = DispatchQueue.global(attributes: .qosDefault)
  main.async {
    print("Result = \(result)")
  }
}

sleep(10000)


//
// let queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0)
// dispatch_async(queue, { () -> () in
//   var result = 0
//   for i in 1...100 {
//     result += i
//   }
//
//   println("Result = \(result)")
// })

