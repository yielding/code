class SampleForHide < UIViewController
  def viewDidLoad
    super

    view.backgroundColor = UIColor.whiteColor
    
    @l0 = UILabel.alloc.initWithFrame([[0, 0], [320, 200]])
    @l0.textAlignment = UITextAlignmentCenter
    @l0.backgroundColor = UIColor.blackColor
    @l0.textColor = UIColor.whiteColor
    @l0.text = "I'm here!"

    self.view.addSubview(@l0)

    button = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    button.frame = [[0, 0], [100, 40]]
    np = view.center
    np.y = view.frame.size.height - 70
    button.center = np

    button.setTitle("Toggle", forState:UIControlStateNormal)
    button.addTarget(self, action: "buttonDidPush", forControlEvents:UIControlEventTouchUpInside)
    view.addSubview(button)
  end

  def buttonDidPush
    @l0.hidden = !@l0.hidden? 
  end

  def touchesEnded(touches, withEvent:e)
    navigationController.setNavigationBarHidden(false, animated:true)
  end
end
