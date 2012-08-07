class SampleForEditableTextView < UIViewController
  @@keyboard_height = 216.0

  def viewDidLoad
    super

    @tv = UITextView.alloc.init 
    @tv.frame = self.view.bounds
    @tv.autoresizingMask = UIViewAutoresizingFlexibleWidth |
                           UIViewAutoresizingFlexibleHeight
    @tv.delegate = self 
    @tv.text = "이 텍스트는 편집할 수 있습니다."

    self.view.addSubview(@tv)
  end

  def viewWillAppear animated
    super animated
    navigationController.setNavigationBarHidden(false, animated:true)
    navigationController.setToolbarHidden(false, animated:true)
  end

  def viewDidAppear animated
    super animated
    self.textViewDidEndEditing @tv
  end

  def viewWillDisappear animated
    super animated
    @tv.resignFirstResponder
  end

  def textViewDidBeginEditing tv
    navigationItem.rightBarButtonItem = 
      UIBarButtonItem.alloc.initWithBarButtonSystemItem(
        UIBarButtonSystemItemDone,
        target:self, 
        action:"done_did_push")

    UIView.beginAnimations(nil, context:nil)  
    UIView.setAnimationDuration 0.3
    
    tv_frame = tv.frame
    tv_frame.size.height = self.view.bounds.size.height - @@keyboard_height
    tv.frame = tv_frame

    toolbar_frame = self.navigationController.toolbar.frame
    toolbar_frame.origin.y =
      view.window.bounds.size.height - toolbar_frame.size.height - @@keyboard_height 
    navigationController.toolbar.frame = toolbar_frame
    UIView.commitAnimations
  end

  def textViewDidEndEditing tv
    navigationItem.rightBarButtonItem = UIBarButtonItem.alloc.initWithBarButtonSystemItem(
        UIBarButtonSystemItemEdit, 
        target:self, 
        action:"edit_did_push")

    UIView.beginAnimations(nil, context: nil)
    UIView.setAnimationDuration 0.3
    tv.frame = self.view.bounds
    toolbar_frame = navigationController.toolbar.frame
    toolbar_frame.origin.y =
      view.window.bounds.size.height - toolbar_frame.size.height
    navigationController.toolbar.frame = toolbar_frame
    UIView.commitAnimations
  end

  def edit_did_push
    @tv.becomeFirstResponder
  end

  def done_did_push
    @tv.resignFirstResponder
  end
end
