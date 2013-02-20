class SampleForStatusbarStyle < UIViewController
  @@statusbar_style = UIStatusBarStyleBlackTranslucent

  def viewDidLoad
    super
    img = UIImage.imageNamed("town.jpg")
    img_view = UIImageView.alloc.initWithImage(img) 
    img_view.frame = self.view.bounds
    img_view.autoresizingMask = UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight 
    img_view.contentMode = UIViewContentModeScaleAspectFill
    self.view.addSubview(img_view)
  end

  def viewWillAppear(animated)
    super(animated)

    self.navigationController.setNavigationBarHidden(true, animated:true)
    self.wantsFullScreenLayout =true
  end

  def touchesEnded(touches, withEvent:event)
    self.change_statusbar_style
  end

  def change_statusbar_style
    app = UIApplication.sharedApplication
    app.setStatusBarStyle(@@statusbar_style, animated:true)

    @@statusbar_style += 1
    if @@statusbar_style > UIStatusBarStyleBlackOpaque
       @@statusbar_style = UIStatusBarStyleDefault 
    end
  end
end
