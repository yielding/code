class SampleForDrag < UIViewController
  KMaximumSteps = 8

  def viewDidLoad
    super
    @should_walk = false

    self.view.backgroundColor = UIColor.whiteColor

    img1 = UIImage.imageNamed("chara1.png")
    img2 = UIImage.imageNamed("chara2.png")

    @char = UIImageView.alloc.initWithImage(img1)
    @char.animationImages = [img1, img2]
    @char.animationDuration = 0.3
    view.addSubview(@char)
  end

  def viewWillDisappear(animated)
    super(animated)
    @should_walk = false
  end

  def touchesBegan(touches, withEvent:event)
    @should_walk = true
    @char.startAnimating
    @target_point  = touches.anyObject.locationInView(self.view)
    self.the_character_will_walk
  end

  def touchesMoved(touches, withEvent:event)
    @target_point = touches.anyObject.locationInView(self.view)
  end

  def touchesEnded(touches, withEvent:event)
    @should_walk = false
    @char.stopAnimating
  end

  def touchesCancelled(touches, withEvent:event)
    @should_walk = false
    @char.stopAnimating
  end

  def the_character_will_walk
    return unless @should_walk

    new_point = @char.center
    if KMaximumSteps < (@target_point.x - new_point.x).abs
      if @target_point.x > new_point.x
        new_point.x += KMaximumSteps
      else
        new_point.x -= KMaximumSteps
      end
    else
      new_point.x = @target_point.x
    end

    if KMaximumSteps < (@target_point.y - new_point.y).abs
      if @target_point.y > new_point.y
        new_point.y += KMaximumSteps
      else
        new_point.y -= KMaximumSteps
      end
    else
      new_point.y = @target_point.y
    end

    @char.center = new_point
    self.performSelector("the_character_will_walk",
                         withObject:nil, 
                         afterDelay:0.3)
  end
end
