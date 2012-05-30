class TitleViewController < UIViewController
  def viewDidLoad
    super
    self.navigationItem.prompt = "슬라이드를 움직이면 화면의 색이 바뀝니다"

    @slider = UISlider.alloc.init
    @slider.frame = self.navigationController.navigationBar.bounds
    @slider.minimumValue = 0.0
    @slider.maximumValue = 1.0
    @slider.value = @slider.maximumValue / 2.0
    @slider.addTarget(self, 
                      action:'sliderDidChanged',
                      forControlEvents:UIControlEventValueChanged)
    self.navigationItem.titleView = @slider

    @label = UILabel.alloc.init
    @label.frame = CGRectInset(self.view.bounds, 10, 10)
    @label.autoresizingMask = UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight
    @label.backgroundColor  = UIColor.blackColor
    self.view.addSubview(@label)
    self.sliderDidChanged
  end

  def viewWillAppear(animated)
    super(animated)
    self.navigationController.setNavigationBarHidden(false, animated:false)
    self.navigationController.setToolbarHidden(false, animated:false)
    self.navigationItem.setHidesBackButton(true, animated:false)
  end

  def touchesEnded(touches, withEvent:event)
    self.navigationItem.setHidesBackButton(false, animated:true) 
  end

  def sliderDidChanged
    v = @slider.value
    color = UIColor.alloc.initWithRed(v, green:v, blue:v, alpha:1.0)
    @label.backgroundColor = color
  end

end
