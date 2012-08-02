class SampleForDataDetectorTypes < UIViewController
  def viewDidLoad
    super

    @tv = UITextView.alloc.init 
    @tv.frame = view.bounds 
    @tv.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight
    @tv.font = UIFont.systemFontOfSize(24)
    @tv.text = "상세한 내용은 이곳을↓\nhttp://www.apple.com/\n연락처: 090-0000-0000\n"
    @tv.dataDetectorTypes = UIDataDetectorTypeAll >> 1
    view.addSubview @tv
  end
end
