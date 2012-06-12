class ModalDialog < UIViewController
  def viewDidLoad
    label = UILabel.alloc.initWithFrame(self.view.bounds)
    label.backgroundColor = UIColor.blackColor
    label.textColor       = UIColor.whiteColor
    label.textAlignment   = UITextAlignmentCenter
    label.text            = "안녕, 나는 모달 윈도우야"
    self.view.addSubview(label)

    btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    btn.setTitle("모달 윈도우", forState:UIControlStateNormal)
    btn.sizeToFit
    new_point = self.view.center
    new_point.y += 80
    btn.center = new_point
    btn.addTarget(self, 
                  action:'goodbyeDidPush', 
                  forControlEvents:UIControlEventTouchUpInside)
    self.view.addSubview(btn)
  end

  def goodbyeDidPush
    self.dismissModalViewControllerAnimated(true)
  end
end
