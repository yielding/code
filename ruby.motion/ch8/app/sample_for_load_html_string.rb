class SampleForLoadHTMLString < UIViewController
  def viewDidLoad
    super
    self.title = "Load HTML String"
    @webview = UIWebView.alloc.init 
    @webview.delegate = self
    @webview.frame    = self.view.bounds 
    @webview.autoresizingMask  = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight
    @webview.dataDetectorTypes = UIDataDetectorTypeAll >> 1
  end

  def viewDidAppear animated
    super animated

    html =  "<b>[전화번호</b> <br />"
             #"010-9735-9421<hr />", 
             #"<b> 홈페이지 </b><br />", 
             #"http://www.apple.com" 
            #].join
    puts html
    @webview.loadHTMLString(html, baseURL:nil)
  end
end
