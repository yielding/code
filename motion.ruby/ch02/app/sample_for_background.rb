class SampleForBackground < UIViewController
  def viewDidLoad
    super
    self.view.backgroundColor = UIColor.blackColor

    @r, @g, @b = Pointer.new(:float, 1), Pointer.new(:float, 1), Pointer.new(:float, 1)
    @l0 = UILabel.alloc.initWithFrame(CGRectMake(0, 0, 320, 200))
    @l0.textAlignment   = UITextAlignmentCenter
    @l0.backgroundColor = UIColor.alloc.initWithRed(@r[0], green:@g[0], blue:@b[0], alpha:1.0)
    @l0.textColor       = UIColor.whiteColor
    @l0.text            = "Change Color"
    view.addSubview(@l0)

    np = view.center
    np.x -= 50; np.y = view.frame.size.height - 70
    red_button = createButton("red", np)
    view.addSubview(red_button)

    np.x += 50
    green_button = createButton("green", np)
    view.addSubview(green_button)

    np.x += 50
    blue_button = createButton("blue", np)
    view.addSubview(blue_button)
  end

  def touchesEnded(touches, withEvent:e)
    navigationController.setNavigationBarHidden(false, animated:true)
  end

  def createButton(title, np)
    button = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    button.frame = CGRectMake(0, 0, 50, 40)
    button.center = np
    button.setTitle(title, forState:UIControlStateNormal)
    colors = { "red"   => UIColor.redColor, 
               "green" => UIColor.greenColor, 
               "blue"  => UIColor.blueColor }
    button.setTitleColor(colors[title], forState:UIControlStateNormal)
    button.addTarget(self,
                     action:"#{title}DidPush",
                     forControlEvents:UIControlEventTouchUpInside)
    button
  end

  def redDidPush
    changeLabelColor(@r)
  end

  def greenDidPush
    changeLabelColor(@g)
  end

  def blueDidPush
    changeLabelColor(@b)
  end

  def changeLabelColor color
    if color[0] > 0.99
      color[0] = 0.0
    else
      color[0] += 0.1
    end

    @l0.backgroundColor = UIColor.alloc.initWithRed(@r[0], green:@g[0], blue:@b[0], alpha:1.0)
  end
end
