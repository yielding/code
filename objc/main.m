#import <Foundation/Foundation.h>

@interface Stack : NSObject {
  int result;
}
- (int) push:(id) x;
- (id)  pop; 
@end

@implementation Stack { NSMutableArray* _array; }
- (id) init {
  if (self = [super init])
    _array = [NSMutableArray array];

  return self;
}

- (void) push:(id) x {
  [_array addObject: x];
}

- (id) pop {
  id x = [_array lastObject];
  return x;
}

@end

int main (int argc, const char * argv[])
{
  @autoreleasepool
  {
    Stack* stack = [[Stack alloc] init];

    [stack push:1];
    // insert code here...
    // NSLog(@"Hello, World! %d", [obj doSomeThing]);

  }

  return 0;
}

