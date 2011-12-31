#import <MacRuby/MacRuby.h>

int main(void) 
{
  auto source 
    = @"module Helper; def self.swapcase(str); str.swapcase; end; end";
  [[MacRuby sharedRuntime] evaluateString:source];

  auto helper 
    = [[MacRuby sharedRuntime] evaluateString:@"Helper"];

  auto result 
    = [helper performRubySelector:@selector(swapcase:)
                    withArguments:@"MacRuby", nil];

  NSLog(@"%@\n", result);
  return 0;
}
