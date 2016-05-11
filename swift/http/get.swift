#!/usr/bin/env swift

import Foundation

let page = "http://macnews.tistory.com/4406"
let req  = NSMutableURLRequest(url: NSURL(string: page)!)
req.httpMethod = "GET"

let task = NSURLSession.shared().dataTask(with: req) { 
  data, response, error in
  guard error == nil && data != nil else {
    print("error =\(error)")
    return
  }

  let rspStr = String(data: data!, encoding: NSUTF8StringEncoding)
  print(rspStr)
}

task.resume()

print("1")
sleep(100)
print("2")
