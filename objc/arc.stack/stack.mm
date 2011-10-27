#import <Foundation/Foundation.h>
#import <Foundation/NSValue.h>

@interface Stack : NSObject { }
- (void) push:(id) x;
- (id)   pop; 
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

- (void) push:(id) x {
  [_array addObject: x];
}

- (id) pop {
  auto x = [_array lastObject];
  return x;
}

@end

int main (int argc, const char * argv[])
{
  @autoreleasepool
  {
    Stack* stack = [[Stack alloc] init];

    for (auto i=0; i<10; i++)
      [stack push:[NSNumber numberWithInt:i]];

    for (auto i=0; i<10; i++)
      NSLog(@"Hello, World! %d", [[stack pop] intValue]);
  }

  return 0;
}
