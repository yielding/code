#import <Cocoa/Cocoa.h>

int main(int argc, char *argv[])
{
  [NSApplication sharedApplication];
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  NSUInteger style = NSTitledWindowMask
                   | NSClosableWindowMask
                   | NSMiniaturizableWindowMask
                   | NSResizableWindowMask;
  NSWindow *window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0,0,400,400)
                                                 styleMask:style
                                                   backing:NSBackingStoreBuffered
                                                     defer:NO];
  [window makeKeyAndOrderFront:nil];
  [pool release];
  [NSApp run];

  return 0;
}
