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
- ( id ) pop; 
@end

@implementation Stack { 
@private  
  NSMutableArray* m_array; 
}

- (id) init
{
  if (self = [super init])
  {
    m_array = [NSMutableArray array];
  }

  return self;
}

- (void) dealloc
{
  NSLog(@"[Stack dealloc]\n");
}

- (void) push:(id) item
{
  [m_array addObject: item];
}

- (id) pop 
{
  auto last = [m_array lastObject];
  [m_array removeLastObject];

  return last;
}

@end

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, const char * argv[])
{
  @autoreleasepool
  {
    auto stack = [[Stack alloc] init];
    auto  iter = 10;
    for (auto i=0; i<iter; i++) [stack push:[NSNumber numberWithInt:i]];
    for (auto i=0; i<iter; i++) 
      NSLog(@"Hello, World! %d", [[stack pop] intValue]);

  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
