class SampleForAutoResize < UIViewController
  def viewDidLoad
    super

    view.backgroundColor = UIColor.blackColor
    @parent = UILabel.alloc.initWithFrame CGRectMake(0, 0, 160, 160)
    @parent.backgroundColor = UIColor.grayColor
    view.addSubview(@parent)

    @child = UILabel.alloc.initWithFrame CGRectInset(@parent.bounds, 30, 30 )
    @child.text = "CHILD 1";
    @child.backgroundColor = UIColor.redColor
    @child.textColor = UIColor.blackColor
    @parent.addSubview @child

    resizeButton = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    resizeButton.frame = CGRectMake( 0, 0, 150, 40 );
    np   = view.center;
    np.y = view.frame.size.height - 40;
    resizeButton.center = np;
    resizeButton.setTitle("Resize", forState:UIControlStateNormal)
    resizeButton.addTarget(self, 
                           action:"resizeDidPush",
                           forControlEvents:UIControlEventTouchUpInside)
    view.addSubview(resizeButton)

    lr = CGRectMake(5, 201, 310, 30)
    addSwitch("FlexibleLeftMargin", lr, "leftDidSwitch:")
    lr.origin = [5, lr.origin.y + 30]
    addSwitch("FlexibleRightMargin", lr, "rightDidSwitch:")
    lr.origin = [5, lr.origin.y + 30]
    addSwitch("FlexibleTopMargin", lr, "topDidSwitch:")
    lr.origin = [5, lr.origin.y + 30]
    addSwitch("FlexibleBottomMargin", lr, "bottomDidSwitch:")
    lr.origin = [5, lr.origin.y + 30]
    addSwitch("FlexibleWidth", lr, "widthDidSwitch:")
    lr.origin = [5, lr.origin.y + 30]
    addSwitch("FlexibleHeight", lr, "heightDidSwitch:")
  end

  def addSwitch(caption, frame, action)
    label = UILabel.alloc.initWithFrame(frame)
    label.text = caption;
    sw = UISwitch.alloc.initWithFrame(CGRectZero)
    np = label.center
    np.x += 100
    sw.center = np
    sw.addTarget(self, action:action, forControlEvents:UIControlEventValueChanged)
    view.addSubview(label)
    view.addSubview(sw)
  end

  def resizeDidPush
    if 200 == @parent.frame.size.width
      @parent.frame = CGRectMake( 0, 0, 160, 160 )
    else
      @parent.frame = CGRectMake( 0, 0, 200, 200 )
    end
  end
  
  def leftDidSwitch(sender)
    if sender.isOn
      @child.autoresizingMask |= UIViewAutoresizingFlexibleLeftMargin
    else
      @child.autoresizingMask &= ~UIViewAutoresizingFlexibleLeftMargin
    end
  end

  def rightDidSwitch(sender)
    if sender.isOn
      @child.autoresizingMask |= UIViewAutoresizingFlexibleRightMargin
    else 
      @child.autoresizingMask &= ~UIViewAutoresizingFlexibleRightMargin
    end
  end

  def topDidSwitch(sender)
    if sender.isOn
      @child.autoresizingMask |= UIViewAutoresizingFlexibleTopMargin
    else
      @child.autoresizingMask &= ~UIViewAutoresizingFlexibleTopMargin
    end
  end

  def bottomDidSwitch(sender)
    if sender.isOn
      @child.autoresizingMask |= UIViewAutoresizingFlexibleBottomMargin
    else
      @child.autoresizingMask &= ~UIViewAutoresizingFlexibleBottomMargin
    end
  end

  def widthDidSwitch(sender)
    if sender.isOn
      @child.autoresizingMask |= UIViewAutoresizingFlexibleWidth
    else
      @child.autoresizingMask &= ~UIViewAutoresizingFlexibleWidth
    end
  end

  def heightDidSwitch(sender)
    if sender.isOn
      @child.autoresizingMask |= UIViewAutoresizingFlexibleHeight
    else
      @child.autoresizingMask &= ~UIViewAutoresizingFlexibleHeight
    end
  end

  def touchesEnded(touches, withEvent:event)
    navigationController.setNavigationBarHidden(false, animated:true)
  end
end
