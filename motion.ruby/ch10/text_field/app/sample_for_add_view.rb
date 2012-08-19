class SampleForAddView < UIViewController
  def viewDidLoad
    super

    self.view.backgroundColor = UIColor.whiteColor
    il = UIImage.imageNamed("leftDog.png")
    ir = UIImage.imageNamed("rightDog.png")

    il_view = UIImageView.alloc.initWithImage(il)
    ir_view = UIImageView.alloc.initWithImage(ir)

    tf1 = UITextField.new
    tf1.borderStyle   = UITextBorderStyleRoundedRect
    tf1.frame         = [[20, 30], [280, 50]]
    tf1.text          = "항상 좌우에 그림 표시"
    tf1.textAlignment = UITextAlignmentCenter
    tf1.contentVerticalAlignment = UIControlContentHorizontalAlignmentCenter
    tf1.leftView      = il_view
    tf1.rightView     = ir_view
    tf1.leftViewMode  = UITextFieldViewModeAlways
    tf1.rightViewMode = UITextFieldViewModeAlways
    self.view.addSubview(tf1)
    
    tf2 = UITextField.new
    tf2.borderStyle   = UITextBorderStyleRoundedRect
    tf2.frame         = [[20, 100], [280, 50]]
    tf2.text          = "비편집 시 오른쪽에 상세 버튼 표시"
    tf2.contentVerticalAlignment = UIControlContentHorizontalAlignmentCenter
    tf2.rightView     = UIButton.buttonWithType(UIButtonTypeDetailDisclosure)
    tf2.rightViewMode = UITextFieldViewModeUnlessEditing
    self.view.addSubview(tf2)
  end
end
