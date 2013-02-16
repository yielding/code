class SampleForAlpha < UIViewController
  def viewDidLoad
    super

    view.backgroundColor = UIColor.whiteColor

    @l0 = UILabel.alloc.initWithFrame([[0, 0], [320, 200]])
    @l0.textAlignment = UITextAlignmentCenter
    @l0.backgroundColor = UIColor.redColor
    @l0.textColor = UIColor.whiteColor
    @l0.adjustsFontSizeToFitWidth = true
    @l0.text = "빨간 배경에 흰 글씨"

    view.addSubview(@l0)

    alphaButton = UIButton.buttonWithType(UIButtonTypeRoundedRect)
    alphaButton.frame = [[0, 0], [100, 40]]
    np   = view.center
    np.y = view.frame.size.height - 70
    alphaButton.center = np

    alphaButton.setTitle("투명화", forState:UIControlStateNormal)
    alphaButton.addTarget(self, action: "alphaDidPush",
                          forControlEvents:UIControlEventTouchUpInside)
    view.addSubview(alphaButton)
  end

  def alphaDidPush
    if @l0.alpha < 0.09
      @l0.alpha  = 1.0
    else
      @l0.alpha -= 0.1
    end
  end

  def touchesEnded(touches, withEvent:e)
    navigationController.setNavigationBarHidden(false, animated:true)
  end

end
