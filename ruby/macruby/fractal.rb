#!/usr/bin/env macruby

framework 'Foundation'
framework 'Cocoa'

class Grid < NSView
  attr_accessor :tiles
  def initWithFrame frame
    super(frame)
    @tiles = []
    self
  end
  def startTiling
    Tile.alloc.initWithFrame(self.frame, view:self).cycle
  end
  def drawRect dirtyRect
    @tiles.enumerateObjectsUsingBlock -> tile, idx, stop do 
      if NSContainsRect(dirtyRect, tile.frame)
        tile.color.set
        NSBezierPath.fillRect tile.frame
      end
    end
  end
end

class Tile < NSObject
  attr_accessor :frame, :color, :grid
  def calculateColor
    size = @frame.size.width
    x = (@frame.origin.x + size / 2 - 308) / 576
    y = (@frame.origin.y + size / 2 - 256) / 576
    2000.times do
      rsq = x*x + y*y
      tempX = x
      x = (2 * x/3 + (x*x - y*y)/(3 * rsq))
      y = (2 * y/3 - (2*tempX * y)/(3 * rsq))
    end
    @color = x > 0 ? NSColor.blueColor : NSColor.redColor  
    @color = NSColor.greenColor if y > 0 
  end
  
  def initWithFrame newFrame, view:contentView
    @color = NSColor.blackColor
    @frame = newFrame
    @grid = contentView
    self
  end
  
  def tileWithX x, Yvalue: y, size: size
    Tile.new.initWithFrame(NSMakeRect(x, y, size, size), view:@grid).cycle
  end
  
  def split
    size = @frame.size.width / 2
    x = @frame.origin.x
    y = @frame.origin.y
    
    Dispatch::Queue.concurrent(:default).async do
      tileWithX x, Yvalue:y, size:size
      tileWithX x, Yvalue:y+size, size:size
      tileWithX x+size, Yvalue:y, size:size
      tileWithX x+size, Yvalue:y+size, size:size
    end
  end
  
  def cycle
    Dispatch::Queue.concurrent(:default).sync{ self.calculateColor }
    Dispatch::Queue.main.async do
      @grid.tiles << self
      @grid.setNeedsDisplayInRect @frame
    end
    self.split if @frame.size.width > 1  
  end
end

class FractalAppDelegate < NSObject
  attr_accessor :window
  def initialize
    self.conformsToProtocol(Protocol.protocolWithName('NSApplicationDelegate'))
  end
  
  def applicationDidFinishLaunching aNotification    
    grid = Grid.alloc.initWithFrame NSMakeRect(0, 0, 512, 512)
    @window.setContentSize grid.frame.size
    @window.contentView = grid
    grid.startTiling
  end
end

application = NSApplication.sharedApplication
NSApplication.sharedApplication.activationPolicy = NSApplicationActivationPolicyRegular
NSApplication.sharedApplication.activateIgnoringOtherApps(true)
application.setDelegate FractalAppDelegate.new

frame = [0.0, 0.0, 100, 300]
window = NSWindow.alloc.initWithContentRect frame, 
                                  styleMask: NSTitledWindowMask | NSClosableWindowMask,
                                    backing: NSBackingStoreBuffered,
                                      defer: false


window.display
window.makeKeyAndOrderFront(nil)
application.delegate.window = window
application.run


