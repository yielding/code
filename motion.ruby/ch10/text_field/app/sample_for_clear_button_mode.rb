class SampleForClearButtonMode < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor

    tf1 = UITextField.new
    tf1.delegate = self
    tf1.clearsOnBeginEditing = true
    tf1.frame = [[20, 20], [280, 30]]
    tf1.borderStyle     = UITextBorderStyleRoundedRect
    tf1.clearButtonMode = UITextFieldViewModeNever
    tf1.text = "UITextFieldViewModeNever"
    view.addSubview(tf1)

    tf2 = UITextField.new
    tf2.delegate = self
    tf2.frame = [[20, 60], [280, 30]]
    tf2.borderStyle     = UITextBorderStyleRoundedRect
    tf2.clearButtonMode = UITextFieldViewModeWhileEditing
    tf2.text = "UITextFieldViewModeWhileEditing"
    view.addSubview(tf2)

    tf3 = UITextField.new
    tf3.delegate = self
    tf3.frame = [[20, 100], [280, 30]]
    tf3.borderStyle     = UITextBorderStyleRoundedRect
    tf3.clearButtonMode = UITextFieldViewModeUnlessEditing
    tf3.text = "UITextFieldViewModeUnlessEditing"
    view.addSubview(tf3)

    tf4 = UITextField.new
    tf4.delegate = self
    tf4.frame = [[20, 140], [280, 30]]
    tf4.borderStyle     = UITextBorderStyleRoundedRect
    tf4.clearButtonMode = UITextFieldViewModeAlways
    tf4.text = "UITextFieldViewModeUnlessAlways"
    view.addSubview(tf4)

    @tfs = [tf1, tf2, tf3, tf4]
  end

  def textFieldShouldClear(tf)
    true
  end
end
