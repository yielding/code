class SampleForBackground < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor
    ip = UIImage.imageNamed("paper.png")
    sp = ip.stretchableImageWithLeftCapWidth(20, topCapHeight:20)

    ig = UIImage.imageNamed("paperGray.png")
    gp = ig.stretchableImageWithLeftCapWidth(20, topCapHeight:20)

    tf = UITextField.new
    tf.delegate = self
    tf.frame = [[20, 100], [280, 50]]
    tf.background = sp
    tf.disabledBackground = gp
    tf.text = "메모"
    tf.textAlignment = UITextAlignmentCenter
    tf.contentVerticalAlignment = UIControlContentHorizontalAlignmentCenter
    view.addSubview(tf)
  end

  def textFieldShouldReturn(tf)
    tf.enabled = false
    true
  end
end
