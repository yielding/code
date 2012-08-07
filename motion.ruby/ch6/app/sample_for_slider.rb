class SampleForSlider < UIViewController
  def viewDidLoad
    super

    title = "UISlider"
    view.backgroundColor = UIColor.whiteColor
    slider = UISlider.alloc.init 
    slider.frame = [[0, 0], [200, 50]]
    slider.minimumValue = 0.0
    slider.maximumValue = 1.0
    slider.center = view.center

    slider.addTarget(self, 
                     action:"sliderDidChange:",
                     forControlEvents:UIControlEventValueChanged)

    @slider_copy = UISlider.alloc.init 
    @slider_copy.frame = slider.frame
    @slider_copy.minimumValue = slider.minimumValue
    @slider_copy.maximumValue = slider.maximumValue
    point = slider.center 
    point.y += 50
    @slider_copy.center = point 

    view.addSubview(slider)
    view.addSubview(@slider_copy)
  end

  def sliderDidChange(sender)
    @slider_copy.value = sender.value
  end
end
