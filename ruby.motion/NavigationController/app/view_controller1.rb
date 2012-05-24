class ViewController1 < UIViewController
  def init
    if super
      self.title = "Hello"
      icon = UIImage.imageNamed("ball1.png")
      self.tabBarItem = UITabBarItem.alloc.initWithTitle("Hello", image:icon, tag:0)
    end
    self
  end

  def viewDidLoad
    label = UILabel.alloc.initWithFrame(self.view.bounds)
    label.text = "Hello world!"
    label.textAlignment = UITextAlignmentCenter
    label.backgroundColor = UIColor.whiteColor
    label.textColor = UIColor.blackColor
    label.autoresizingMask = UIViewAutoresizingFlexibleWidth or 
                             UIViewAutoresizingFlexibleHeight
    self.view.addSubview(label)
  end
end
