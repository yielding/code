class SampleForNetworkActivityIndicator < UIViewController
  def viewWillAppear animated
    super animated
    app = UIApplication.sharedApplication
    app.networkActivityIndicatorVisible = true
  end

  def viewWillDisappear animated
    super animated
    app = UIApplication.sharedApplication
    app.networkActivityIndicatorVisible = false
  end
end
