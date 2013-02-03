class AppDelegate
  def application(application, didFinishLaunchingWithOptions:launchOptions)
    bounds = UIScreen.mainScreen.bounds
    @window = UIWindow.alloc.initWithFrame(bounds)
    root_vc = ConvexHullViewController.alloc.init
    @navi = UINavigationController.alloc.initWithRootViewController(root_vc)
    @navi.setNavigationBarHidden(false, animated:true)
    #@window.addSubview(@navi.view)
    @window.setRootViewController(@navi)
    @window.makeKeyAndVisible
    true
  end
end
