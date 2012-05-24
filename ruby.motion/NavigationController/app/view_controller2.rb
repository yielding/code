class ViewController2 < UIViewController
  def init
    if super
      self.title = "안녕"
      icon = UIImage.imageNamed("ball2.png")
      self.tabBarItem = UITabBarItem.alloc.initWithTitle("안녕", image:icon, tag:0)
    end
    self
  end

  def viewDidLoad
    label = UILabel.alloc.initWithFrame(self.view.bounds)
    label.text = "안녕 세계!"
    label.textAlignment    = UITextAlignmentCenter
    label.backgroundColor  = UIColor.blackColor
    label.textColor        = UIColor.whiteColor
    label.autoresizingMask = UIViewAutoresizingFlexibleWidth or 
                             UIViewAutoresizingFlexibleHeight
    self.view.addSubview(label)
  end
end
