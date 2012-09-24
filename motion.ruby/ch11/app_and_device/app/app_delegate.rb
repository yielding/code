class AppDelegate
  def application(application, didFinishLaunchingWithOptions:launchOptions)
    frame   = UIScreen.mainScreen.bounds
    @window = UIWindow.alloc.initWithFrame(frame)
    rc = RootViewController.new
    @navi = UINavigationController.alloc.initWithRootViewController(rc)
    @navi.setNavigationBarHidden(true, animated:false)

    @window.addSubview(@navi.view)
    @window.makeKeyAndVisible
    true
  end

  def application(app, handleOpenURL:url)
    return false if url.nil? or url.query.nil?

    alert = UIAlertView.new
    alert.message = query
    alert.addButtonWithTitle("OK")
    alert.show
    true
  end
end
