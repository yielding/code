#!/usr/local/bin/macruby 

framework 'Cocoa'
framework 'QuartzCore'

class FireworkDelegate
  attr_accessor :window
  def initWithURL(url)
    case url
    when String
       @firework_photo_url = NSURL.URLWithString(url)
    when NSURL
      @firework_photo_url = url
    else
      raise "The AppDelegate class was initiated with an unknown type object"
    end
    self
  end
  
  def applicationDidFinishLaunching(aNotification)
    window.delegate = self
    view = window.contentView
    
    view.wantsLayer = true
    @root_layer = CALayer.layer #create the root layer
    
    #configure the layer attributes
    @root_layer.bounds = view.frame
    color = CGColorCreateGenericRGB(0.0, 0.0, 0.0, 1)
    @root_layer.backgroundColor = color
    CGColorRelease(color)
    
    # Load the spark image for the particle
    data_provider = CGDataProviderCreateWithURL(@firework_photo_url)
    image = CGImageCreateWithPNGDataProvider(data_provider, nil, false, KCGRenderingIntentDefault)
    
    @mortor = CAEmitterLayer.layer
    @mortor.emitterPosition = CGPointMake(320, 0)
    @mortor.renderMode = KCAEmitterLayerAdditive
    
    # Invisible particle representing the rocket before the explosion
    rocket = CAEmitterCell.emitterCell
    rocket.emissionLongitude = Math::PI / 2
    rocket.emissionLatitude = 0
    rocket.lifetime = 1.6
    rocket.birthRate = 1
    rocket.velocity = 400
    rocket.velocityRange = 100
    rocket.yAcceleration = -250
    rocket.emissionRange = Math::PI  / 4
    color = CGColorCreateGenericRGB(0.5, 0.5, 0.5, 0.5)
    rocket.color = color
    CGColorRelease(color);
    rocket.redRange = 0.5
    rocket.greenRange = 0.5
    rocket.blueRange = 0.5
    
    # Name the cell so that it can be animated later using keypath
    rocket.name = "rocket"
    
    # Flare particles emitted from the rocket as it flys
    flare = CAEmitterCell.emitterCell
    flare.contents = image
    flare.emissionLongitude = (4 * Math::PI) / 2
    flare.scale = 0.4
    flare.velocity = 100
    flare.birthRate = 45
    flare.lifetime = 1.5
    flare.yAcceleration = -350
    flare.emissionRange = Math::PI / 7
    flare.alphaSpeed = -0.7
    flare.scaleSpeed = -0.1
    flare.scaleRange = 0.1
    flare.beginTime = 0.01
    flare.duration = 0.7
    
    # The particles that make up the explosion
    firework = CAEmitterCell.emitterCell
    firework.contents = image
    firework.birthRate = 9999
    firework.scale = 0.6
    firework.velocity = 130
    firework.lifetime = 2
    firework.alphaSpeed = -0.2
    firework.yAcceleration = -80
    firework.beginTime = 1.5
    firework.duration = 0.1
    firework.emissionRange = 2 * Math::PI
    firework.scaleSpeed = -0.1
    firework.spin = 2
    
    # Name the cell so that it can be animated later using keypath
    firework.name = "firework"
    
    # preSpark is an invisible particle used to later emit the spark
    preSpark = CAEmitterCell.emitterCell
    preSpark.birthRate = 80
    preSpark.velocity = firework.velocity * 0.70
    preSpark.lifetime = 1.7
    preSpark.yAcceleration = firework.yAcceleration * 0.85
    preSpark.beginTime = firework.beginTime - 0.2
    preSpark.emissionRange = firework.emissionRange
    preSpark.greenSpeed = 100
    preSpark.blueSpeed = 100
    preSpark.redSpeed = 100
    
    # Name the cell so that it can be animated later using keypath
    preSpark.name = "preSpark"
    
    # The 'sparkle' at the end of a firework
    spark = CAEmitterCell.emitterCell
    spark.contents = image
    spark.lifetime = 0.05
    spark.yAcceleration = -250
    spark.beginTime = 0.8
    spark.scale = 0.4
    spark.birthRate = 10
    
    preSpark.emitterCells = [spark]
    rocket.emitterCells = [flare, firework, preSpark]
    @mortor.emitterCells = [rocket]
    
    @root_layer.addSublayer @mortor
        
    view.layer = @root_layer
    view.needsDisplay = true # Force the view to update
    
    window.center
    window.orderFrontRegardless
  end
  
  def applicationShouldTerminateAfterLastWindowClosed the_application
    true
  end
  def applicationShouldTerminate sender
    NSLog("Finished")
    NSTerminateNow
  end  
end

application = NSApplication.sharedApplication
NSApplication.sharedApplication.activationPolicy = NSApplicationActivationPolicyRegular
application.delegate = FireworkDelegate.alloc.initWithURL(ARGV.shift || "http://dl.dropbox.com/u/930741/tspark.png")
# create the Application Window
frame = [0.0, 0.0, 660, 500]
window = NSWindow.alloc.initWithContentRect frame, 
                                  styleMask: NSTitledWindowMask | NSClosableWindowMask,
                                    backing: NSBackingStoreBuffered,
                                      defer: false

application.delegate.window = window
window.orderOut(nil)
window.display
puts "Starting the app..."
application.run
