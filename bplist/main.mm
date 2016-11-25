#import <Foundation/Foundation.h>

#include <iostream>

int main(int argc, char** argv)
{
  @autoreleasepool {
    auto path = [[[NSBundle mainBundle] bundlePath] 
      stringByAppendingPathComponent:@"metadata.plist"];

    NSLog(@"path: %@", path);

    auto info = [NSMutableDictionary dictionaryWithContentsOfFile:path]; 
    NSLog(@"%@", [info objectForKey:@"MCMMetadataIdentifier"]);
  }

  return 0;
}
