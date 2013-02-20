class SampleForTextViewObserving < UIViewController
  KEYBOARD_HEIGHT = 216.0

  def viewDidLoad
    super
    @tv = UITextView.alloc.init 
    @tv.frame = self.view.bounds 
    @tv.delegate = self 
    @tv.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight
    @tv.text = "이 텍스르를 편집하세요"
    self.view.addSubview(@tv)
  end

  def viewDidAppear animated
    super animated
    self.textViewDidEndEditing(@tv)
  end

  def viewWillDisappear animated
    super animated
    @tv.resignFirstResponder
  end

  def textView(tv, shouldChangeTextInRange:range, replacementText:text)
    puts "shouldChangeTextInRange #{text}"
    text != "a"
  end

  def textViewShouldBeginEditing tv
    puts "textViewShouldEndEditing"
    true
  end

  def textViewShouldEndEditing tv
    puts "textViewShouldEndEditing"
    true
  end

  def textViewDidChangeSelection tv
    puts "textViewDidChangeSelection"
  end

  def textViewDidChange tv
    puts "textViewDidChange"
  end
  
  def textViewDidBeginEditing tv
    puts "textViewDidBeginEditing"
    
    self.navigationItem.rightBarButtonItem = 
      UIBarButtonItem.alloc.initWithBarButtonSystemItem(
          UIBarButtonSystemItemDone,
          target:self, 
          action:"doneDidPush")
    UIView.beginAnimations(nil, context:nil)
    UIView.setAnimationDuration 0.3 
    # make UiTextView smaller so as not to be hidden by the keyboard
    tv_frame = tv.frame 
    tv_frame.size.height = self.view.bounds.size.height - KEYBOARD_HEIGHT
    tv.frame = tv_frame 

    # drag up toolbar
    tb_frame = self.navigationController.toolbar.frame 
    tb_frame.origin.y = 
      self.view.window.bounds.size.height - tb_frame.size.height - KEYBOARD_HEIGHT
    self.navigationController.toolbar.frame = tb_frame
    UIView.commitAnimations
  end

  def textViewDidEndEditing tv
    NSLog "textViwDidEndEditing"

    self.navigationItem.rightBarButtonItem = 
      UIBarButtonItem.alloc.initWithBarButtonSystemItem(UIBarButtonSystemItemEdit, 
          target:self, 
          action:"editDidPush")
    UIView.beginAnimations(nil, context:nil)
    UIView.setAnimationDuration 0.3 

    tv.frame = self.view.bounds
    tb_frame = self.navigationController.toolbar.frame
    tb_frame.origin.y = 
      self.view.window.bounds.size.height - tb_frame.size.height 
    self.navigationController.toolbar.frame = tb_frame 
    UIView.commitAnimations
  end
    
  def editDidPush
    @tv.becomeFirstResponder
  end
    
  def doneDidPush
    @tv.resignFirstResponder
  end
end
