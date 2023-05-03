#import <Foundation/Foundation.h>
#import <Foundation/NSValue.h>

#include <stdexcept>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
@interface Queue: NSObject { }
- (void) push: (id) item;
- (void) enque:(id) item;
- (id)   deque;
- (id)   shift; 
- (id)   shift2; 
- (void) clear;
@end

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
@implementation Queue { 
@private  
  NSMutableArray* m_array; 
}

- (id) init {
  if (self = [super init])
    m_array = [NSMutableArray array];

  return self;
}

- (void) dealloc {
  NSLog(@"[Stack dealloc]\n");
}

- (void) push:(id) item {
  [m_array addObject: item];
}

- (void) enque:(id) item {
  [self push:item];
}

- (id) shift2 {
  if ([m_array count] == 0)
  {
    auto e = [NSException exceptionWithName: @"HotTea"
                                     reason: @"tea is too hot"
                                   userInfo: nil];
    @throw e;
  }

  auto item = [m_array objectAtIndex: 0];
  [m_array removeObjectAtIndex: 0];

  return item;
}

- (id) shift {
  if ([m_array count] == 0)
    throw std::runtime_error("queue is empty");

  auto item = [m_array objectAtIndex: 0];
  [m_array removeObjectAtIndex:0];

  return item;
}

- (void) clear {
  [m_array removeAllObjects];
}

- (id) deque {
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
  @autoreleasepool {
    auto queue = [[Queue alloc] init];
    auto iter  = 10;
    for (auto i=0; i<iter; i++) [queue push:[NSNumber numberWithInt:i]];

    [queue clear];

    /*
    try
    {
      for (auto i=0; i<iter; i++) 
        NSLog(@"Hello, World! %d", [[queue shift] intValue]);
    }
    catch (std::exception& e)
    {
      NSLog(@"Error reason: %s\n", e.what());
    }
    */

    @try {
      NSLog(@"Hello %d\n", [[queue shift2] intValue]);
    } @catch (NSException* e) {
      NSLog(@"cating %@ reason %@", [e name], [e reason]);
    } @finally {
      NSLog(@"finally");
    }
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
