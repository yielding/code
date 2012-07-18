class SampleForDoubleTap < UIViewController

  def viewDidLoad
    super
    self.view.backgroundColor = UIColor.whiteColor
  end

  def touchesBegan(touches, withEvent:event)
    puts "touchesBegan"
    @single_tap_ready = false
  end

  def touchesEnded(touches, withEvent:event)
    # always 1 or 0
    tap_count = touches.anyObject.tapCount
    puts "tap count = #{tap_count}"

    if 2 > tap_count
      @single_tap_ready = true
      self.performSelector(:"singleTap", withObject:nil, afterDelay:1.0)
    else
      self.performSelector(:"doubleTap")
    end
  end

  def singleTap
    return unless @single_tap_ready
    puts "single tap"

    alert = UIAlertView.alloc.initWithTitle(nil, 
             message:"Single Tap",
             delegate:nil,
             cancelButtonTitle:nil, 
             otherButtonTitles:"OK", nil)
    alert.show
  end

  def doubleTap
    puts "double tap"
    alert = UIAlertView.alloc.initWithTitle(nil, 
             message:"Double Tap",
             delegate:nil,
             cancelButtonTitle:nil, 
             otherButtonTitles:"OK", nil)
    alert.show
  end
end
