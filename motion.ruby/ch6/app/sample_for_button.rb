class SampleForButton < UIViewController
  def viewDidLoad
    super
    title = "UIButton"
    view.backgroundColor = UIColor.whiteColor

    button = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    button.setTitle("건희 바보!", forState:UIControlStateNormal)
    button.sizeToFit
    button.center = self.view.center 

    button.autoresizingMask = 
      UIViewAutoresizingFlexibleWidth or
      UIViewAutoresizingFlexibleHeight or
      UIViewAutoresizingFlexibleLeftMargin or
      UIViewAutoresizingFlexibleRightMargin or
      UIViewAutoresizingFlexibleTopMargin or
      UIViewAutoresizingFlexibleBottonMargin

    button.addTarget(self, action:"buttonDidPush:", 
                     forControlEvents:UIControlEventTouchUpInside)
    view.addSubview(button)
  end

  def buttonDidPush(sender)
    if sender.class == UIRoundedRectButton
      button = sender
      a = UIAlertView.alloc.initWithTitle(nil,
                                          message:button.currentTitle,
                                          delegate:nil,
                                          cancelButtonTitle:nil,
                                          otherButtonTitles:"OK")
      a.show
    end
  end
end
