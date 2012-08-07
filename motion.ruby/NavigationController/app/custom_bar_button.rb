class CustomBarButton < UIViewController
  def viewDidLoad
    super
    self.title = "CustomBarButton"

    image = UIImage.imageNamed "face.jpg"
    image_view = UIImageView.alloc.initWithImage image
    icon       = UIBarButtonItem.alloc.initWithCustomView(image_view)
    navigationController.rightBarButtonItem = icon
    
    switch_ = UISwitch.alloc.init 
    switch_.on = true
    sb_button  = UIBarButtonItem.alloc.initWithCustomView(switch_)
    
  end

  def viewWillAppear(animated)
    super(animated)
    navigationController.setNavigationBarHidden(false, animated:false)
    navigationController.setToolbarHidden(false, animated:false)
  end
end
