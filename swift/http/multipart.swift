//
//  HttpMultipart.swift
//  multipart
//
//  Created by yielding on 2016. 7. 13..
//  Copyright © 2016년 com.hancomgmd. All rights reserved.
//

import Foundation

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
extension Data {
  mutating func append(_ str: String) {
    let d = str.data(using: String.Encoding.utf8, allowLossyConversion: true)
    append(d!)
  }
}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
public class HttpMultipart {
  
  private var uploadUrl: String
  
  private var filePath, filePathKey: String
  
  private var mimeType: String
  
  private var params: [String: String] = [:]
  
  private let crlf  = "\r\n"
  private let crlf2 = "\r\n\r\n"
  
  public init?(uploadUrl url: String) {
    if url.characters.count < 5 { return nil }
    
    uploadUrl   = url
    mimeType    = "application/x-sqlite3"
    filePath    = ""
    filePathKey = ""
    
  }
  
  ///
  /// - Parameters:
  ///   - key: name that will be used in server
  ///   - value : actual value for the given key
  ///
  /// - Returns:
  ///   blah
  ///
  public func addValue(key: String, value: String) -> HttpMultipart {
    params[key] = value

    return self
  }
  
  ///
  /// - Parameters:
  ///   - key: name that will be used in server
  ///   - value : actual value for the given key
  ///
  /// - Returns:
  ///   blah
  ///
  public func addFile(key: String, filePath fn: String) -> Bool {
    if !FileManager.default.fileExists(atPath: fn) {
      return false
    }
    
    filePath = fn
    filePathKey = key
    
    return true
  }

  private func makeBody(boundary: String) -> Data {
    var body = Data()
    
    let fileData = try! NSData(contentsOfFile: filePath) as Data
    
    for (key, value) in params {
      body.append("--\(boundary)\(crlf)")
      body.append("Content-Disposition: form-data; name=\"\(key)\"\(crlf2)")
      body.append("\(value)\(crlf)")
    }
    
    //
    // TODO
    // identify mime-type of the upload file from 'fileData
    //
    if !filePath.isEmpty {
      body.append("--\(boundary)\(crlf)")
      body.append("Content-Disposition: form-data; name=\"\(filePathKey)\"; filename=\"\(filePath)\"\(crlf)")
      body.append("Content-Type: \(mimeType)\(crlf2)")
      body.append(fileData)
      body.append(crlf)
    }
    
    body.append("--\(boundary)--\(crlf)")
    
    return body
  }
  
  ///
  /// - Parameters:
  ///   - key: name that will be used in server
  ///   - value : actual value for the given key
  ///
  /// - Returns:
  ///   blah
  ///
  public func upload() -> Bool {
    if uploadUrl.isEmpty { return false }
    
    let bd = "Boundary-\(UUID().uuidString)"
    
    var req = URLRequest(url: URL(string: uploadUrl)!)
    req.setValue("multipart/form-data; boundary=\(bd)", forHTTPHeaderField: "Content-Type")
    req.httpMethod = "POST"
    req.httpBody   = makeBody(boundary: bd)
    
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
    
    return true
  }
}

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
/*
 let page = "http://172.16.3.208/datafiles"
 var uploader = HttpMultipart(uploadUrl: page)!
 
 _ = uploader.addValue(key: "datafile[ver_id]", value: "11")
 .addValue(key: "datafile[node_id]", value: "11")
 _ = uploader.addFile (key: "datafile[file_path]", filePath: "/tmp/maptile.db")
 _ = uploader.upload()
 
 sleep(10)
 print("ok")
 
 */
