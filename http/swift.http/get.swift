#!/usr/bin/env swift

import Foundation

let page = "http://macnews.tistory.com/4406"
var req  = URLRequest(url: URL(string: page)!)
req.httpMethod = "GET"

let task = URLSession.shared.dataTask(with: req) {
  data, response, error in
  guard error == nil && data != nil else {
    print("error =\(error)")
    return
  }
  
  guard let rspStr = String(data: data!, encoding: String.Encoding.utf8) else {
    return
  }

  print(rspStr)
}

task.resume()

print("1")
sleep(100)
print("2")
