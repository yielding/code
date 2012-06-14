class Rotate < UIViewController
  def viewDidLoad
    super

    self.title = "Rotate"
    self.view.backgroundColor = UIColor.blackColor

    image = UIImage.imageNamed("town.jpg")
    image_view = UIImageView.alloc.initWithImage(image)
    image_view.frame = [[30, 0], [240, 240]]
    image_view.contentMode = UIViewContentModeScaleAspectFit;
    self.view.addSubview(image_view)
  end

  def viewWillAppear(animated)
    super(animated)

    navigationController.setNavigationBarHidden(false, animated:false)
    navigationController.setToolbarHidden(false, animated:false)
  end

  def shouldAutorotateToInterfaceOrientation(ori)
    true
  end
end
