class SampleForAccelerometer < UIViewController
  def viewDidLoad
    super 

    self.view.backgroundColor = UIColor.whiteColor

    img = UIImage.imageNamed("metal.png")
    @img_view = UIImageView.alloc.initWithImage(img)
    @img_view.center = self.view.center 
    @img_view.autoresizingMask =
      UIViewAutoresizingFlexibleLeftMargin or 
      UIViewAutoresizingFlexibleRightMargin or 
      UIViewAutoresizingFlexibleTopMargin or
      UIViewAutoresizingFlexibleBottomMargin

    self.view.addSubview(@img_view)
  end

  def viewWillAppear(animated)
    super(animated)
    @speedx = @speedy = 0.0
    accelerometer = UIAccelerometer.sharedAccelerometer
    accelerometer.updateInterval = 1.0 / 60.0  # 60 Hz
    accelerometer.delegate = self
  end

  def viewWillDisappear(animated)
    super(animated)

    accelerometer = UIAccelerometer.sharedAccelerometer
    accelerometer.delegate = nil
  end

  def accelerometer(accelerometer, didAccelerate:acceleration)
    @speedx += accelerometer.x
    @speedy -= accelerometer.y

    posx = @img_view.center.x + @speedx;
    posy = @img_view.center.y - @speedy;

    if posx < 0.0
      posx = 0.0
      @speedx *= -0.4
    elsif posx > self.view.bounds.size.width
      posx = self.view.bounds.size.width
      @speedx *= -0.4
    end

    if posy < 0.0
      posy = 0.0
      @speedy = 0.0
    elsif posy > self.view.bounds.size.height
      posy = self.view.bounds.size.height
      @speedy *= -1.5
    end

    @img_view.center = [ posx, posy ]
  end
end
