class SampleForAdjustFontSize < UIViewController
  def viewDidLoad
    super

    self.view.backgroundColor = UIColor.whiteColor
    tf1 = UITextField.new
    tf1.delegate = self
    tf1.frame    = [[20, 50], [280, 50]]
    tf1.borderStyle = UITextBorderStyleRoundedRect
    tf1.text = "폰트 사이즈 조정"
    tf1.adjustsFontSizeToFitWidth = true
    tf1.font = UIFont.systemFontOfSize(48)
    tf1.contentVerticalAlignment = UIControlContentHorizontalAlignmentCenter
    view.addSubview(tf1)
  end

  def textFieldShouldReturn(tf)
    if tf.canResignFirstResponder
      tf.resignFirstResponder
    end
  end
end
