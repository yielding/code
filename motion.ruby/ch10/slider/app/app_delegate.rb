class AppDelegate
  def application(application, didFinishLaunchingWithOptions:launchOptions)
    frame   = UIScreen.mainScreen.bounds
    @window = UIWindow.alloc.initWithFrame(frame)
    rc = RootViewController.new
    @navi = UINavigationController.alloc.initWithRootViewController(rc)
    @window.addSubview(@navi.view)
    @window.makeKeyAndVisible
    true
  end
end
