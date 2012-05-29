class FullScreenWithTransparent < UIViewController
  def viewDidLoad
    super
    image = UIImage.imageNamed("town.jpg")
    image_view = UIImageView.alloc.initWithImage(image)
    image_view.autoresizingMask = UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight
    image_view.contentMode = UIViewContentModeScaleAspectFill
    self.view.addSubview(image_view)
  end

  def viewWillAppear(animated)
    super(animated)

    @fullscreen = false
    self.navigationController.setNavigationBarHidden(false, animated:false)
    self.navigationController.setToolbarHidden(false, animated:false)

    app = UIApplication.sharedApplication
    app.statusBarStyle = UIStatusBarStyleBlackTranslucent

    self.navigationController.navigationBar.barStyle = UIBarStyleBlack
    self.navigationController.navigationBar.translucent = true;
    self.navigationController.toolbar.barStyle = UIBarStyleBlack
    self.navigationController.toolbar.translucent = true
    self.wantsFullScreenLayout = true
  end

  def touchesEnded(touches, withEvent:event)
    @fullscreen = @fullscreen ? false : true

    need_animation = true
    if need_animation
      UIView.beginAnimations(nil, context:nil)
      UIView.setAnimationDuration(0.3)
    end

    UIApplication.sharedApplication.setStatusBarHidden(@fullScreen, withAnimation:need_animation)
   
    self.navigationController.navigationBar.alpha = @fullScreen ? 0.0 : 1.0;
    self.navigationController.toolbar.alpha       = @fullScreen ? 0.0 : 1.0;

    UIView.commitAnimations if need_animation
  end
end
