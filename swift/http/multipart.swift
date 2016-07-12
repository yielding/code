//
//  main.swift
//  multipart
//
//  Created by yielding on 2016. 7. 12..
//  Copyright © 2016년 com.hancomgmd. All rights reserved.
//

/*
 http://172.16.3.208/datafiles
 action="/datafiles" accept-charset="UTF-8" method="post" enctype="multipart/form-data"
 */

import Foundation


extension Data {
  mutating func append(_ str: String) {
    let d = str.data(using: String.Encoding.utf8, allowLossyConversion: true)
    append(d!)
  }
}

func generateBoundary() -> String {
  return "Boundary-\(UUID().uuidString)"
}

func createBodyWithParameters(filePathKey: String, fileName: String, boundary: String) -> Data {
  var body = Data()
  
  let fileData = try! NSData(contentsOfFile: fileName) as Data
  
  body.append("--\(boundary)\r\n")
  body.append("Content-Disposition: form-data; name=\"datafile[ver_id]\"\r\n\r\n")
  body.append("11\r\n")
  
  body.append("--\(boundary)\r\n")
  body.append("Content-Disposition: form-data; name=\"datafile[node_id]\"\r\n\r\n")
  body.append("11\r\n")

  
  let mimetype = "image/jpg"
  
  body.append("--\(boundary)\r\n")
  body.append("Content-Disposition: form-data; name=\"datafile[\(filePathKey)]\"; filename=\"\(fileName)\"\r\n")
  body.append("Content-Type: \(mimetype)\r\n\r\n")
  body.append(fileData)
  body.append("\r\n")
  
  body.append("--\(boundary)--\r\n")
  
  return body
}

func upload() {
  let page = "http://172.16.3.208/datafiles"
  let boundary = generateBoundary()
  
  var req = URLRequest(url: URL(string: page)!)
  req.setValue("multipart/form-data; boundary=\(boundary)", forHTTPHeaderField: "Content-Type")
  req.httpMethod = "POST"
  req.httpBody   = createBodyWithParameters(filePathKey: "file_path", fileName: "/tmp/maptile.db", boundary: boundary)
  
  let task = URLSession.shared.dataTask(with: req) {
    data, response, error in
    guard error == nil && data != nil else {
      print("error=\(error)")
      return
    }
    
    let response = String(data: data!, encoding: String.Encoding.utf8)!
    print(response)
  }
  
  task.resume()
}


print("Hello, World!")

upload()

sleep(100)
