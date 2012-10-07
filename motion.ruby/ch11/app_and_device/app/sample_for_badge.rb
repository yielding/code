class SampleForBadge < UIViewController
  def viewDidLoad
    super
    @label = UILabel.alloc.init
    @label.frame = self.view.bounds
    @label.frame.origin.x -= 100
    @label.autoresizingMask = UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight
    @label.textAlignment   = NSTextAlignmentCenter
    @label.backgroundColor = UIColor.blackColor
    @label.textColor = UIColor.whiteColor
    @label.font = UIFont.systemFontOfSize(128)

    self.view.addSubview(@label)
    @app = UIApplication.sharedApplication
  end

  def viewWillAppear animated
    super
    @badge_no = UIApplication.sharedApplication.applicationIconBadgeNumber
    self.updateLabel(@label, withNumber:@badge_no)
  end

  def viewWillDisappear animated
    super
    @app.applicationIconBadgeNumber = @badge_no
  end

  def touchesEnded(touches, withEvent:event)
    if touches.anyObject.tapCount > 1
      @badge_no = 0
    else
      @badge_no += 1
    end

    self.updateLabel(@label, withNumber:@badge_no)
  end

  def updateLabel(la, withNumber:no)
    la.text = no.to_s
    @app.applicationIconBadgeNumber = no
  end
end
