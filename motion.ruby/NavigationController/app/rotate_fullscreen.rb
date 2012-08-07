class RotateAndFullScreen < UIViewController
  def viewDidLoad
    super
    image = UIImage.imageNamed("town.jpg")
    iview = UIImageView.alloc.initWithImage(image)
    iview.frame = self.view.bounds
    iview.autoresizingMask = UIViewAutoresizingFlexibleWidth or 
                             UIViewAutoresizingFlexibleHeight
    iview.contentMode = UIViewContentModeScaleAspectFill
    self.view.addSubview(iview)
  end

  def viewWillAppear(animated)
    super animated
    @fullscreen = false

    navigationController.setNavigationBarHidden(false, animated:false)
    navigationController.setToolbarHidden(false, animated:false)

    app = UIApplication.sharedApplication
    app.statusBarStyle = UIStatusBarStyleBlackTranslucent
    navigationController.navigationBar.barStyle = UIBarStyleBlack;
    navigationController.navigationBar.translucent = true;
    navigationController.toolbar.barStyle    = UIBarStyleBlack;
    navigationController.toolbar.translucent = true;
    wantsFullScreenLayout = true
  end

  def shouldAutorotateToInterfaceOrientation(ori)
    true
  end

  def touchesEnded(touches, withEvent:event)
    @fullscreen = !@fullscreen
    need_animation = true

    if need_animation
      UIView.beginAnimations(nil, context:nil)
      UIView.setAnimationDuration(0.3)
    end

    app = UIApplication.sharedApplication
    app.setStatusBarHidden(@fullscreen, withAnimation:need_animation)
    self.navigationController.navigationBar.alpha = @fullscreen ? 0.0 : 1.0;
    self.navigationController.toolbar.alpha       = @fullscreen ? 0.0 : 1.0;

    UIView.commitAnimations if need_animation

    unless @fullscreen 
      self.navigationController.setNavigationBarHidden(true, animated:false)
      self.navigationController.setToolbarHidden(true, animated:false)
      self.navigationController.setNavigationBarHidden(false, animated:false)
      self.navigationController.setToolbarHidden(false, animated:false)
    end
  end

end
