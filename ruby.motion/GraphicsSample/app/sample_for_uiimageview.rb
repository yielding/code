class SampleForUIImageView < UIViewController
  def viewDidLoad
    super 
    image = UIImage.imageNamed("dog.jpg")
    image_view = UIImageView.alloc.initWithImage(image)
    image_view.center = self.view.center 
    image_view.autoresizingMask =
      UIViewAutoresizingFlexibleTopMargin or 
      UIViewAutoresizingFlexibleBottomMargin
    view.addSubview(image_view)
  end
end
