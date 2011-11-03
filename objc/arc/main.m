//
//  main.m
//  test_objc_interface
//
//  Created by Lee Chang Ha on 11. 10. 10..
//  Copyright (c) 2011년 heaven. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "MyObj.h"

int main (int argc, const char * argv[])
{
  @autoreleasepool {
    MyObj* obj = [[MyObj alloc] init];
    
    // insert code here...
    NSLog(@"Hello, World! %d", [obj doSomeThing]);
  }

  return 0;
}

