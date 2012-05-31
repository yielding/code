#!/usr/bin/env macruby

framework "Cocoa"

class CustomView < NSView

  def temp_context(&block)
    context = NSGraphicsContext.currentContext
    context.saveGraphicsState
    yield
    context.restoreGraphicsState
  end

  def drawRect(r)
    temp_context do
      NSColor.redColor.set
      NSBezierPath.fillRect(r)
    end

    p = NSBezierPath.bezierPath
    p.lineWidth = 2

    p.moveToPoint [100, 50]

    p.lineToPoint [100, 100]
    p.lineToPoint [200, 100]
    p.lineToPoint [200, 50]

    p.closePath

    temp_context do
      NSColor.colorWithCalibratedWhite(0.9, alpha: 0.5).set
      NSBezierPath.fillRect([100, 50, 100, 50])
    end

    p.stroke

    temp_context do
      shadow = NSShadow.alloc.init
      shadow.shadowOffset = [4, -4]
      shadow.set
      font = NSFont.fontWithName("Helvetica", size:24)
      attr = { NSFontAttributeName => font, 
               NSForegroundColorAttributeName => NSColor.whiteColor }
      "Macruby Rocks".drawAtPoint([60, 120], withAttributes: attr)
    end
  end
  
end

app    = NSApplication.sharedApplication
frame  = [0.0, 0.0, 300, 200]
mask   = NSTitledWindowMask | NSClosableWindowMask;

window = NSWindow.alloc.initWithContentRect(frame,
          styleMask: mask,
            backing: NSBackingStoreBuffered,
              defer: false)

content_view = CustomView.alloc.initWithFrame(frame)
window.contentView = content_view

window.display
window.makeKeyAndOrderFront(nil)
window.orderFrontRegardless

app.run
