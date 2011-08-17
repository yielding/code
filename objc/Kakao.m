#import <Cocoa/Cocoa.h>

int main(int argc, char *argv[])
{
  [NSApplication sharedApplication];
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  NSUInteger style = NSTitledWindowMask
                   | NSClosableWindowMask
                   | NSMiniaturizableWindowMask
                   | NSResizableWindowMask;
  NSWindow *window 
    = [[NSWindow alloc] initWithContentRect:NSMakeRect(0,0,400,400)
                                  styleMask:style
                                    backing:NSBackingStoreBuffered
                                      defer:NO];
  [window makeKeyAndOrderFront:nil];
<<<<<<< HEAD

  [pool drain];
=======
  [pool release];
>>>>>>> 9f9f7772d76b4192ce77d5b35929227da092e293
  [NSApp run];

  return 0;
}
