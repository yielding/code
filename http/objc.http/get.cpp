#include <iostream>
#include <string>
#include <vector>

#import <Foundation/Foundation.h>

using namespace std;

int main(int argc, char *argv[])
{
  auto url = [NSURL URLWithString:@"http://www.32133.com/test?name=xx"];
  auto data = [NSData dataWithContentsOfURL:url];
  auto ret = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
  NSLog(@"ret=%@", ret);

  return 0;
}

/*

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
*/
