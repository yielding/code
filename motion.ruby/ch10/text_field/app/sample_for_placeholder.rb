class SampleForPlaceholder < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor

    tf = UITextField.new
    tf.frame = [[20, 100], [280, 30]]
    tf.borderStyle = UITextBorderStyleRoundedRect
    tf.contentVerticalAlignment = UIControlContentVerticalAlignmentCenter
    tf.placeholder = "메시지를 입력하셈"
    self.view.addSubview(tf)
  end
end
