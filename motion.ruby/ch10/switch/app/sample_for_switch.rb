class SampleForSwitch < UIViewController
  def viewDidLoad
    super

    self.view.backgroundColor = UIColor.whiteColor
    @sw1 = UISwitch.new
    @sw1.center = [100, 50]
    @sw1.on = true
    @sw1.addTarget(self, action:"switchDidChange", 
                   forControlEvents:UIControlEventValueChanged)
    self.view.addSubview(@sw1)

    @sw2 = UISwitch.new
    @sw2.center = [100, 100]
    @sw2.on = false
    self.view.addSubview(@sw2)
  end

  def switchDidChange
    @sw2.setOn(!@sw2.on?, animated:true)
  end
end
