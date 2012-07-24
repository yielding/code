class SampleForSecureTextEntry < UIViewController
  def viewDidLoad
    super
    @tv = UITextField.new 
    @tv.frame = view.bounds
    @tv.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight
    @tv.backgroundColor  = UIColor.whiteColor
    @tv.secureTextEntry  = true 
    view.addSubview(@tv)
  end

  def viewDidAppear animated
    super animated

    @tv.becomeFirstResponder
  end
end
