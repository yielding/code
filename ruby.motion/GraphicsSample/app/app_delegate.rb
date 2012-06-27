class AppDelegate
  def application(application, didFinishLaunchingWithOptions:launchOptions)
    bounds  = UIScreen.mainScreen.bounds
    @window = UIWindow.alloc.initWithFrame(bound)
    @root_view_controller = RootViewController.alloc.init 
    @navi = UINavigationController.alloc.initWithFrootViewController(@root_view_controller)
    @window.addSubView(@navi.view)
    @window.makeKeyAndVisible
    true
  end
end
