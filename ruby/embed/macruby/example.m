#import <MacRuby/MacRuby.h>

int main(void) 
{
  NSString *source =  @"module Helper; def self.swapcase(str); str.swapcase; end; end";
  [[MacRuby sharedRuntime] evaluateString:source];

  id helper = [[MacRuby sharedRuntime] evaluateString:@"Helper"];

  NSString *result 
    = [helper performRubySelector:@selector(swapcase:)
                    withArguments:@"MacRuby", nil];

  NSLog(@"%@\n", result);
  return 0;
}
