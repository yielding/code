class SampleForChangeColorAndFont < UIViewController
  def viewDidLoad
    super

    view.backgroundColor = UIColor.whiteColor
    tf = UITextField.new
    tf.frame = [[20, 100], [280, 50]]
    tf.borderStyle = UITextBorderStyleBezel
    tf.backgroundColor = UIColor.blackColor
    tf.textColor       = UIColor.redColor
    tf.textAlignment   = UITextAlignmentCenter
    tf.font = UIFont.systemFontOfSize(36)
    tf.text = "UITextFields"
    self.view.addSubview(tf)
  end
end
