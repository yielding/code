class SampleForWebViewLoadData < UIViewController
  def viewDidLoad
    super
    self.title = "Load Data"

    @webview = UIWebView.alloc.init 
    @webview.delegate = self 
    @webview.frame    = self.view.bounds
    @webview.autoresizingMask = 
      UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight

    self.view.addSubview(@webview)

    @ac = UIActivityIndicatorView.alloc.init 
    @ac.frame = [[0, 0], [20, 20]]
    indicator = UIBarButtonItem.alloc.initWithCustomView(@ac)
    adjustmnt = UIBarButtonItem.alloc.initWithBarButtonSystemItem(UIBarButtonSystemItemFlexibleSpace,
                  target:nil, 
                  action:nil)
    self.setToolbarItems([indicator, adjustmnt], animated:true)
  end

  def viewDidAppear animated
    super animated

    path = NSBundle.mainBundle.pathForResource("sample", ofType:"pdf")
    unless path.nil?
      data = NSData.dataWithContentsOfFile path 
      @webview.loadData(data, MIMEType:"application/pdf", textEncodingName:nil, baseURL:nil)
    else 
      puts "File not found."
    end
  end

  def update_control_enabled
    @webview.loading? ? @ac.startAnimating : @ac.stopAnimating
  end

  def webViewDidStartLoad wv
    puts "webViewDidFinishLoad"
    update_control_enabled
  end

  def webViewDidFinishLoad wv
    puts "webViewDidFinishLoad"
    update_control_enabled
  end

  def webView(wv, didFailLoadWithError:err)
    puts "didFailLoadWithError: #{err}"
    puts "#{err.localizedDescription}"
    update_control_enabled
  end
end
