class AppDelegate
  def application(application, didFinishLaunchingWithOptions:launchOptions)
    bounds   = UIScreen.mainScreen.bounds
    @window  = UIWindow.alloc.initWithFrame(bounds)
    @topMenu = TopMenuController.alloc.init
    @root    = UINavigationController.alloc.initWithRootViewController(@topMenu)
    @window.addSubview(@root.view)
    @window.makeKeyAndVisible
    true
  end
end
