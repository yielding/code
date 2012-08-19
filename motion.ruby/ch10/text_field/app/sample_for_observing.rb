class SampleForObserving < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor

    tf1 = UITextField.new
    tf1.delegate = self
    tf1.frame = [[20,50], [280, 40]]
    tf1.borderStyle = UITextBorderStyleRoundedRect
    tf1.text = "NSLog로 delegate 확인"
    tf1.clearButtonMode = UITextFieldViewModeAlways
    tf1.contentVerticalAlignment = UIControlContentHorizontalAlignmentCenter
    view.addSubview(tf1)
  end

  # callbacks
  def textFieldShouldBeginEditing(tf)
    NSLog("textFieldDidBeginEditing %@", tf.text)
  end

  def textFieldDidBeginEditing(tf)
    NSLog("textFieldDidEndEditing %@", tf.text)
  end

  def textFieldShouldEndEditing(tf)
    NSLog("textFieldShouldEndEditing %@", tf.text)
  end

  def textFieldDidEndEditing(tf)
    NSLog("textFieldDidEndEditing %@", tf.text)
  end

  def textField(tf, shouldChangeCharactersInRange:rng, replacementString:str)
    NSLog("shouldChangeCharactersInRange %@", str)
    true
  end

  def textFieldShouldClear(tf)
    NSLog("textFieldShouldClear %@", tf.text)
    true
  end

  def textFieldShouldReturn(tf)
    NSLog("textFieldShouldReturn %@", tf.text)
    true
  end
end
