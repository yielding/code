class AppDelegate
  def application(application, didFinishLaunchingWithOptions:launchOptions)
    bounds  = UIScreen.mainScreen.bounds
    @window = UIWindow.alloc.initWithFrame(bounds)
    rc = RootViewController.alloc.init
    navigationController = UINavigationController.alloc.initWithRootViewController(rc)
    navigationController.setNavigationBarHidden(true, animated:false)
    @window.rootViewController = navigationController
    @window.makeKeyAndVisible
    true
  end
end
