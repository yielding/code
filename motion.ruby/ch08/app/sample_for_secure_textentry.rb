class SampleForSecureTextEntry < UIViewController
  def viewDidLoad
    super

    @tf = UITextField.new 
    @tf.frame = view.bounds
    @tf.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight
    @tf.backgroundColor  = UIColor.whiteColor
    @tf.text = "password"
    @tf.secureTextEntry = true
    view.addSubview(@tf)
  end

  def viewDidAppear animated
    super animated
    @tf.becomeFirstResponder
  end
end
