class SampleForTripleTap < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor
  end

  def touchesBegan(touches, withEvent:event)
    @single_tap_ready = false
    @double_tap_ready = false
  end

  def touchesEnded(touches, withEvent:event)
    tapCount = touches.anyObject.tapCount
    if 2 > tapCount
      @single_tap_ready = true
      performSelector(:"singleTap",
                      withObject:nil,
                      afterDealy:0.3)
    elsif 3 > tabCount
      @double_tap_ready = true
      performSelector(:"doubleTap",
                      withObject:nil,
                      afterDealy:0.3)
    else
      performSelector(:"tripleTap")
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
    return unless @double_tap_ready

    alert = UIAlertView.alloc.initWithTitle(nil, 
              message:"Double Tap",
              delegate:nil,
              cancelButtonTitle:nil, 
              otherButtonTitles:"OK", nil)

    alert.show
  end

  def tripleTap
    alert = UIAlertView.alloc.initWithTitle(nil, 
              message:"Triple Tap",
              delegate:nil,
              cancelButtonTitle:nil, 
              otherButtonTitles:"OK", nil)

    alert.show
  end
end
