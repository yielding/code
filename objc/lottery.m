#import <Foundation/Foundation.h>

int main(int argc, char const* argv[])
{
  NSAutoreleasePool *po = [[NSAutoreleasePool alloc] init];
  NSMutableDictionary *dict = [NSMutableDictionary dictionaryWithCapacity:10];

  NSLog(@"leech");
  NSLog(@"%s", [dict description]);

  [po drain];
  
  return 0;
}
