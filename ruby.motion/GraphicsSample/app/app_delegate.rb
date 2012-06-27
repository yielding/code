class AppDelegate
  def application(application, didFinishLaunchingWithOptions:launchOptions)
    bounds  = UIScreen.mainScreen.bounds
    @window = UIWindow.alloc.initWithFrame(bounds)
    @root_view_controller = RootViewController.alloc.init 
    @navi = UINavigationController.alloc.initWithRootViewController(@root_view_controller)
    @window.addSubview(@navi.view)
    @window.makeKeyAndVisible
    true
  end
end
