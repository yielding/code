class SampleForDoubleTap < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor
  end

  def touchesBegan(touches, withEvent:event)
    @single_tap_ready = false
  end

  def touchesEnded(touches, withEvent:event)
    # always 1 or 0
    tap_count = touches.anyObject.tapCount
    if tap_count < 2
      @single_tap_ready = true
      self.performSelector("singleTab", withObject:nil, afterDealy:0.3)
    else
      self.performSelector("doubleTag")
    end
  end

  def singleTab
    return unless @single_tap_ready
    alert = UIAlertView.alloc.initWithTitle(nil, 
              message:"Single Tap",
              delegate:nil,
              cancelButtonTitle:nil, 
              otherButtonTitles:"OK", nil)

    alert.show
  end

  def doubleTag
    alert = UIAlertView.alloc.initWithTitle(nil, 
              message:"Double Tap",
              delegate:nil,
              cancelButtonTitle:nil, 
              otherButtonTitles:"OK", nil)
    alert.show
  end
end
