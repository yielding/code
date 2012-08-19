class SampleForSimple < UIViewController
  def viewDidLoad
    super
    view.backgroundColor = UIColor.whiteColor

    tf1 = UITextField.new 
    tf1.delegate    = self
    tf1.frame       = [[20, 20], [280, 30]]
    tf1.borderStyle = UITextBorderStyleLine
    tf1.text = "UITextBorderStyleLine"
    tf1.returnKeyType = UIReturnKeyNext
    self.view.addSubview(tf1)

    tf2 = UITextField.new 
    tf2.delegate    = self
    tf2.frame       = [[20, 60], [280, 30]]
    tf2.borderStyle = UITextBorderStyleBezel
    tf2.text = "UITextBorderStyleBezel"
    tf2.returnKeyType = UIReturnKeyNext
    self.view.addSubview(tf2)

    tf3 = UITextField.new 
    tf3.delegate    = self
    tf3.frame       = [[20, 100], [280, 30]]
    tf3.borderStyle = UITextBorderStyleRoundedRect
    tf3.text = "UITextBorderStyleRoundedRect"
    tf3.returnKeyType = UIReturnKeyNext
    self.view.addSubview(tf3)

    tf4 = UITextField.new 
    tf4.delegate    = self
    tf4.frame       = [[20, 140], [280, 30]]
    tf4.borderStyle = UITextBorderStyleNone
    tf4.text = "UITextBorderStyleRoundedNone"
    tf4.returnKeyType = UIReturnKeyNext
    self.view.addSubview(tf4)

    @tfs = [tf1, tf2, tf3, tf4]
  end

  def textFieldDidBeginEditing(tf)
    @cur_field_index = @tfs.find_index(tf)
  end

  def textFieldShouldReturn(tf)
    @cur_field_index += 1
    @cur_field_index = 0 if @tf.count <= @cur_field_index

    new_tf = @tfs[@cur_field_index]
    if new_tf.canBecomeFirstResponder
       new_tf.becomeFirstResponder
    end
  end
end
