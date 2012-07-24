class SampleForWebViewSimple < UIViewController
  def viewDidLoad
    super 
    title = "통신 중 상태를 표시한다"
    @webview = UIWebView.alloc.init 
    @webview.delegate = self
    @webview.frame    = self.view.bounds
    @webview.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight
    self.view.addSubview(@webview)
    @activity = UIActivityIndicatorView.new 
    @activity.frame = [[0, 0], [20, 20]]
    indicator  = UIBarButtonItem.alloc.initWithCustomView(@activity)
    adjustment = UIBarButtonItem.alloc.initWithBarButtonSystemItem(UIBarButtonSystemItemFlexibleSpace,
                    target:nil,
                    action:nil)
    buttons = [adjustment, indicator, adjustment]
    self.setToolbarItems(buttons, animated:true)

  end

  def viewDidAppear animated
    super animated

    req = NSURLRequest.requestWithURL(NSURL.URLWithString("http://www.apple.com"))
    @webview.loadRequest(req)
  end

  def webViewDidStartLoad wv
    @activity.startAnimating
  end

  def webViewDidFinishLoad wv
    @activity.stopAnimating
  end

  def webView(wv, didFailLoadWithError:error)
    @activity.stopAnimating
  end
end
