class AppDelegate
  def application(application, didFinishLaunchingWithOptions:launchOptions)

    root_vc = ConvexHullViewController.alloc.init
    @navi   = UINavigationController.alloc.initWithRootViewController(root_vc)
    @navi.setNavigationBarHidden(false, animated:true)

    @window = UIWindow.alloc.initWithFrame(UIScreen.mainScreen.bounds)
    @window.rootViewController = @navi
    @window.makeKeyAndVisible

    true
  end
end
