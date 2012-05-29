class ModalDialog < UIViewController
  def viewDidLoad
    label = UILabel.alloc.initWithFrame(self.view.bounds)
    label.backgroundColor = UIColor.blackColor
    label.textColor = UIColor.whiteColor
    label.textAlignment = UITextAlignmentCenter;
    label.text = "안녕, 나는 모달윈도우야"
    self.view.addSubview(label)

    btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    btn.setTitle("Good-bye" forState:UIControlStateNormal)
    btn.sizeToFit
    new_point = self.view.center
  end

  def goodbyeDidPush
  end
end
