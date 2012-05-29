class ModalDialog < UIViewController
  def viewDidLoad
    super
    label = UILabel.alloc.initWithFrame(self.view.bounds)
    label.backgroundColor = UIColor.blackColor
    label.textColor = UIColor.whiteColor
    label.textAlignment = UITextAlignmentCenter
    label.text = "안녕, 나는 모달윈도우야"
    self.view.addSubview(label)

    btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    btn.setTitle("Good-bye", forState:UIControlStateNormal)
    btn.sizeToFit
    new_point = self.view.center
    new_point.y += 80
    btn.center = new_point
    btn.addTarget(self, action:'goodbyeDidPush', forControlEvents:UIControlEventTouchUpInside)
    self.view.addSubview(btn)
  end

  def goodbyeDidPush
    self.dismissModalViewControllerAnimated(true)
  end
end

class ModalDialogViewController < UIViewController
  def viewDidLoad
    super

    self.view.backgroundColor = UIColor.whiteColor
    modal_btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    modal_btn.setTitle("모달 대화상자 호출", forState:UIControlStateNormal)
    modal_btn.sizeToFit
    modal_btn.center = self.view.center
    modal_btn.addTarget(self, action:'modalDidPush', forControlEvents:UIControlEventTouchUpInside)
    self.view.addSubview(modal_btn)
  end

  def modalDidPush
    dialog = ModalDialog.alloc.init 
    dialog.modalTransitionStyle = UIModalTransitionStyleCoverVertical
    self.presentModalViewController(dialog, animated:true)
  end
end
