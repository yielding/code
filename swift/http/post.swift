#!/usr/bin/env swift

import Foundation

let str = "create table Message(col TEXT)"
let paramStr = "app[table]=Message&app[script]=\(str)"

// work/rails/schema_mgr
let page = "http://localhost:3000/apps" 
var req  = URLRequest(url: URL(string: page)!)
req.httpMethod = "POST"
req.httpBody   = paramStr.data(using: String.Encoding.utf8)

let task = URLSession.shared.dataTask(with: req) {
  data, response, error in
  guard error == nil && data != nil else {
    print("error =\(error)")
    return
  }
  
  let rspStr = String(data: data!, encoding: String.Encoding.utf8)!
  print(rspStr)
}

task.resume()

print("1")
sleep(100)
print("2")
