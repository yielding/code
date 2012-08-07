class SampleForDrag < UIViewController
  KMaximumSteps = 16

  def viewDidLoad
    super

    @should_walk = false

    self.view.backgroundColor = UIColor.whiteColor
    img1  = UIImage.imageNamed("chara1.png")
    img2  = UIImage.imageNamed("chara2.png")
    @char = UIImageView.alloc.initWithImage(img1)
    @char.animationImages   = [img1, img2]
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

    np = @char.center
    if KMaximumSteps < (@target_point.x - np.x).abs
      if @target_point.x > np.x
        np.x += KMaximumSteps
      else
        np.x -= KMaximumSteps
      end
    else
      np.x = @target_point.x
    end

    if KMaximumSteps < (@target_point.y - np.y).abs
      if @target_point.y > np.y
        np.y += KMaximumSteps
      else
        np.y -= KMaximumSteps
      end
    else
      np.y = @target_point.y
    end

    @char.center = np
    self.performSelector("the_character_will_walk",
                         withObject:nil, 
                         afterDelay:0.3)
  end
end
