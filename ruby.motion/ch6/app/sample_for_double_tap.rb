class SampleForDoubleTap < UIViewController

  def viewDidLoad
    super
    self.view.backgroundColor = UIColor.whiteColor
  end

  def touchesBegan(touches, withEvent:event)
    @single_tap_ready = false
  end

  def touchesEnded(touches, withEvent:event)
    # always 1 or 0
    tap_count = touches.anyObject.tapCount
    puts "tap count = #{tap_count}"

    if 2 > tap_count
      @single_tap_ready = true
      self.performSelector(:"singleTap", withObject: nil, afterDealy: 0.6)
    else
      self.performSelector(:"doubleTap")
    end
  end

  def singleTap
    return unless @single_tap_ready
    alert = UIAlertView.alloc.initWithTitle(nil, 
              message:"Single Tap",
              delegate:nil,
              cancelButtonTitle:nil, 
              otherButtonTitles:"OK", nil)

    alert.show
  end

  def doubleTap
    alert = UIAlertView.alloc.initWithTitle(nil, 
              message:"Double Tap",
              delegate:nil,
              cancelButtonTitle:nil, 
              otherButtonTitles:"OK", nil)
    alert.show
  end
end
