#!/usr/bin/env swift

import Foundation

let str = "create table Message(col TEXT)"
let paramStr = "app[table]=Message&app[script]=\(str)"

let page = "http://localhost:3000/apps"
let req  = NSMutableURLRequest(url: NSURL(string: page)!)
req.httpMethod = "POST"
req.httpBody   = paramStr.data(using: NSUTF8StringEncoding)

let task = NSURLSession.shared().dataTask(with: req) { 
  data, response, error in
  guard error == nil && data != nil else {
    print("error =\(error)")
    return
  }

  let rspStr = NSString(data: data!, encoding: NSUTF8StringEncoding)
  print(rspStr)
}

task.resume()

print("1")
sleep(100)
print("2")
