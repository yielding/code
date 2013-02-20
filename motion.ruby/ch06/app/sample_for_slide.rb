class SampleForSlide < UIViewController
  KNeedMove = 10 
  KSlideNone = 1
  KSlideHorizontal = 2
  KSlideVertical = 3

  def viewDidLoad
    super
    self.view.backgroundColor = UIColor.blackColor
    @label = UILabel.alloc.init 
    @label.frame = self.view.bounds
    @label.backgroundColor = UIColor.whiteColor
    @label.textAlignment   = UITextAlignmentCenter
    @label.text = "상하좌우 슬라이드 가능"
    @label.autoresizingMask = 
      UIViewAutoresizingFlexibleWidth or 
      UIViewAutoresizingFlexibleHeight

    self.view.addSubview(@label)

    noti = NSNotificationCenter.defaultCenter
    noti.addObserver(self, 
                     selector: :"suspend", 
                     name: UIApplicationWillResignActiveNotification,
                     object: nil)
  end

  def touchesBegan(touches, withEvent:event)
    @touch_began  = touches.anyObject.locationInView(self.view)
    @label_origin = @label.center
    @direction    = KSlideNone
  end

  def touchesMoved(touches, withEvent:event)
    point = touches.anyObject.locationInView(self.view)
    dx = point.x - @touch_began.x
    dy = point.y - @touch_began.y

    if @direction == KSlideNone
      if dx.abs > dy.abs
        @direction = KSlideHorizontal if KNeedMove <= dx.abs
      else
        @direction = KSlideVertical if KNeedMove <= dy.abs
      end
    end

    if @direction != KSlideNone
      new_point = @label_origin
      if KSlideHorizontal == @direction
        new_point.x += dx 
      else
        new_point.y += dy
      end
      @label.center = new_point
    end
  end

  def touchesEnded(touches, withEvent:event)
    UIView.beginAnimations(nil, context:nil)
    @label.center = self.view.center 
    UIView.commitAnimations
  end

  def touchesCancelled(touches, withEvent:event)
    self.touchesEnded(touches, withEvent:event) 
  end

  def suspend
    self.touchesCancelled(nil, withEvent:nil) 
  end
end
