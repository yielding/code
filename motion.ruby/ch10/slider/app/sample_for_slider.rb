class SampleForSlider < UIViewController
  def viewDidLoad
    super

    @label = UILabel.alloc.init
    @label.frame = self.view.bounds
    @label.autoresizingMask = UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight
    @label.text = "0.5"
    @label.font = UIFont.boldSystemFontOfSize(36)
    @label.textAlignment = UITextAlignmentCenter
    self.view.addSubview(@label)

    @slider = UISlider.alloc.init
    @slider.frame = [[0, 0], [250, 50]]
    @slider.minimumValue = 0.0
    @slider.maximumValue = 1.0
    @slider.value  = 0.5
    @slider.center = self.view.center
    @slider.addTarget(self, 
                      action: :"sliderDidChange:", 
                      forControlEvents:UIControlEventValueChanged)
    self.view.addSubview(@slider)
  end

  def sliderDidChange(sender)
    @label.text = sprintf("%0.1f", sender.value) if sender.class == UISlider
  end

end
