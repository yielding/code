class SampleForWebView < UIViewController
  def viewDidLoad
    super 
    self.title = "웹뷰 테스트"

    @wv = UIWebView.alloc.init
    @wv.delegate = self
    @wv.frame = self.view.bounds

    @wv.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight
    @wv.scalesPageToFit = true 
    self.view.addSubview(@wv)

    @reload_btn = UIBarButtonItem.alloc.initWithBarButtonSystemItem(
        UIBarButtonSystemItemRefresh,
        target:self,
        action:"reloadDidPush")

    @stop_btn = UIBarButtonItem.alloc.initWithBarButtonSystemItem(
        UIBarButtonSystemItemStop,
        target:self,
        action:"stopDidPush")

    @back_btn = UIBarButtonItem.alloc.initWithTitle("Back",
         style:UIBarButtonItemStyleBordered,
        target:self,
        action:"backDidPush")

    @forward_btn = UIBarButtonItem.alloc.initWithTitle("Forward",
         style:UIBarButtonItemStyleBordered,
        target:self,
        action:"forwardDidPush")

    buttons = [@reload_btn, @stop_btn, @back_btn, @forward_btn]
    self.setToolbarItems(buttons, animated:true)
  end

  def reloadDidPush
    @wv.reload
  end
      
  def stopDidPush
    @wv.stopLoading if @wv.loading?
  end
     
  def backDidPush
    @wv.goBack if @wv.canGoBack
  end

  def forwardDidPush
    @wv.goForward if @wv.canGoForward
  end
      
  def updateControlEnabled
    UIApplication.sharedApplication.networkActivityIndicatorVisible = @wv.loading? 
    @stop_btn.enabled = @wv.loading?
    @back_btn.enabled = @wv.canGoBack
    @forward_btn.enabled = @wv.canGoForward
  end

  def viewDidAppear animated
    super animated
    req = NSURLRequest.requestWithURL(NSURL.URLWithString("http://www.apple.com"))
    @wv.loadRequest(req)
    self.updateControlEnabled
  end

  def viewWillDisappear animated
    super animated
    UIApplication.sharedApplication.networkActivityIndicatorVisible = false
  end

  def webViewDidStartLoad wv
    self.updateControlEnabled
  end

  def webViewDidFinishLoad wv
    self.updateControlEnabled
  end

  def webView(wv, didFailLoadWithError:error)
    self.updateControlEnabled
  end
end
