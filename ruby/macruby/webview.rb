framework 'Cocoa'
framework 'WebKit'

application = NSApplication.sharedApplication

# create the window
width  = 800.0
height = 600.0
frame  = [0.0, 0.0, width, height]
mask = NSTitledWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask
window = NSWindow.alloc.initWithContentRect(frame,
          styleMask:mask,
          backing:NSBackingStoreBuffered,
          defer:false)

# assign a content view instance
content_view = NSView.alloc.initWithFrame(frame)
window.contentView = content_view

# create a web view positioned in the top left quarter of the super view
web_view_frame = [0.0, height/2, width/2, height/2]
web_view = WebView.alloc.initWithFrame(web_view_frame, frameName: "Web Frame", groupName: nil)
request = NSURLRequest.requestWithURL(NSURL.URLWithString("http://macruby.org"))
web_view.mainFrame.loadRequest(request)
content_view.addSubview(web_view)

# center the window
screen_frame = NSScreen.mainScreen.frame
window.frameOrigin = [(screen_frame.size.width - width)/2.0, ((screen_frame.size.height - height)/2.0)]

# show the window
window.display
window.makeKeyAndOrderFront(nil)
window.orderFrontRegardless

application.run

