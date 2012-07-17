class AppDelegate
  def application(application, didFinishLaunchingWithOptions:launchOptions)
    frame   = UIScreen.mainScreen.bounds
    @window = UIWindow.alloc.initWithFrame(frame)
    root_view_controller = RootViewController.alloc.init 
    @navi = UINavigationController.alloc.initWithRootViewController(root_view_controller)
    @navi.setNavigationBarHidden(true, animated:false)
    @window.addSubview(@navi.view)
    @window.makeKeyAndVisible
    true
  end
end
