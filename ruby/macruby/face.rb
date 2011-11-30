#!/usr/bin/env macruby
framework 'Cocoa'
framework 'QuartzCore'

class NSColor 
  def toCGColor
    colorRGB = self.colorUsingColorSpaceName NSCalibratedRGBColorSpace    

    components = Array.new(4){Pointer.new(:double)}
    
    colorRGB.getRed components[0], green:components[1], blue:components[2], alpha:components[3]
    
    components.collect!{|x| x[0] }
    
    theColorSpace = CGColorSpaceCreateWithName(KCGColorSpaceGenericRGB)
    theColor = CGColorCreate(theColorSpace, components)
    CGColorSpaceRelease(theColorSpace)
    theColor
  end
end

# NSVIew set background color & and set center
class NSView

  # setbackground like on IOS
  def setBackgroundColor aColor
    viewLayer = CALayer.layer
    viewLayer.setBackgroundColor aColor.toCGColor
    self.setWantsLayer true # // view's backing store is using a Core Animation Layer
    self.setLayer viewLayer
  end

  # helper to set nsview center like on IOS
  def setCenter aPoint
    self.setFrameOrigin [aPoint.x-(self.frame.size.width/2), aPoint.y-(self.frame.size.height/2)]
    self.setNeedsDisplay true
  end
end

# NSImage to CGImage
class NSImage 
  def toCGImage
    source = CGImageSourceCreateWithData(self.TIFFRepresentation, nil)
    maskRef = CGImageSourceCreateImageAtIndex(source, 0, nil)
  end
end

class FaceDetectionDelegate
  attr_accessor :aWindow

  def initialize
    self.conformsToProtocol(Protocol.protocolWithName('NSApplicationDelegate'))
  end
  
  def applicationDidFinishLaunching aNotification
    photo_url = NSURL.URLWithString "http://b2cloud.com.au/wp-content/uploads/2011/10/steves.jpg"
    image = NSImage.alloc.initWithContentsOfURL photo_url
    
    # Helpers to set the NSImageView size
    bitmap = NSBitmapImageRep.imageRepWithData image.TIFFRepresentation
    unless (bitmap == nil)
      NSLog("rep size: w=#{bitmap.pixelsWide}, h=#{bitmap.pixelsHigh}\n")
    end
    
    @imageView = NSImageView.alloc.init
    @imageView.setImage image
    @imageView.setWantsLayer true  
    @imageView.setImageScaling NSImageScaleProportionallyUpOrDown
    @imageView.layer.setAffineTransform CGAffineTransformMakeScale(-1, 1)
    @imageView.setImageFrameStyle NSImageFramePhoto
    
    @aWindow.setFrame NSMakeRect(0.0, 0.0, (bitmap.pixelsWide+20), (bitmap.pixelsHigh+20)), display:true, animate:true 
    @aWindow.center
    
    @aWindow.contentView.setWantsLayer true
    @aWindow.contentView.layer.setAffineTransform CGAffineTransformMakeScale(1, 1)
    
    @imageView.setFrame CGRectMake(0.0, 0.0, bitmap.pixelsWide, bitmap.pixelsHigh)
    @aWindow.contentView.addSubview @imageView
    detect_faces
  end
  
  def detect_faces
    ciImage = CIImage.imageWithCGImage @aWindow.contentView.subviews.last.image.toCGImage    
    detectorOptions = {CIDetectorAccuracy: CIDetectorAccuracyHigh }
    detector = CIDetector.detectorOfType "CIDetectorTypeFace", context:nil, options:detectorOptions
    features = detector.featuresInImage(ciImage)
    features.each do |feature|
      #CGContextFillRect(context, feature.bounds);
      face = NSView.alloc.initWithFrame feature.bounds
      face.setBackgroundColor NSColor.yellowColor.colorWithAlphaComponent(0.4)
      @aWindow.contentView.addSubview face
      
      if(feature.hasLeftEyePosition)
        left_eye = NSView.alloc.initWithFrame CGRectMake(0, 0, 15, 15)
        left_eye.setBackgroundColor NSColor.blueColor.colorWithAlphaComponent(0.2)
        left_eye.setCenter feature.leftEyePosition
        @aWindow.contentView.addSubview left_eye
      end
      
      if(feature.hasRightEyePosition)
        right_eye = NSView.alloc.initWithFrame CGRectMake(0, 0, 15, 15)
        right_eye.setBackgroundColor NSColor.redColor.colorWithAlphaComponent(0.2)
        right_eye.setCenter feature.rightEyePosition
        @aWindow.contentView.addSubview right_eye          
      end
      
      if(feature.hasMouthPosition)
        mouth = NSView.alloc.initWithFrame CGRectMake(0, 0, 40, 15)
        mouth.setBackgroundColor NSColor.greenColor.colorWithAlphaComponent(0.2)
        mouth.setCenter feature.mouthPosition
        @aWindow.contentView.addSubview mouth          
      end
    end
  end
end

# Create the Application
application = NSApplication.sharedApplication
application.setDelegate FaceDetectionDelegate.new

# create the Application Window
frame = [0.0, 0.0, 330, 250]
window = NSWindow.alloc.initWithContentRect frame, 
                                  styleMask: NSTitledWindowMask | NSClosableWindowMask,
                                    backing: NSBackingStoreBuffered,
                                      defer: false

window.display
window.makeKeyAndOrderFront(nil)

application.delegate.aWindow = window
application.run
