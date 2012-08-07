class ModalViewController < UIViewController
  def viewDidLoad
    self.view.backgroundColor = UIColor.whiteColor

    modal_btn = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    modal_btn.setTitle("모달 대화상자를 호출", forState:UIControlStateNormal)
    modal_btn.sizeToFit
    modal_btn.center = self.view.center
    modal_btn.addTarget(self, action:'modalDidPush:',
                        forControlEvents:UIControlEventTouchUpInside)
    self.view.addSubview(modal_btn)
  end

  def modalDidPush(sender)
    dialog = ModalDialog.alloc.init
    dialog.modalTransitionStyle = UIModalTransitionStyleCoverVertical
    self.presentModalViewController(dialog, animated:true)
  end
end
