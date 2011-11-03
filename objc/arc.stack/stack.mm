#import <Foundation/Foundation.h>
#import <Foundation/NSValue.h>

#include <iostream>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
@interface Stack: NSObject { }
- (void) push:(id)x;
-   (id) pop; 
@end

@implementation Stack { 
@private  
  NSMutableArray* _array; 
}

- (id) init {
  if (self = [super init])
    _array = [NSMutableArray array];

  return self;
}

- (void) dealloc {
  // std::cout << "이창하leech\n";
  NSLog(@"[Stack dealloc]\n");
}

- (void) push:(id) x {
  [_array addObject: x];
}

- (id) pop {
  auto x = [_array lastObject];
  return x;
}

@end

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main (int argc, const char * argv[])
{
  @autoreleasepool {
    auto stack = [[Stack alloc] init];

    int const iter = 2;
    for (auto i=0; i<iter; i++) [stack push:[NSNumber numberWithInt:i]];
    for (auto i=0; i<iter; i++) NSLog(@"Hello, World! %d", [[stack pop] intValue]);
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
