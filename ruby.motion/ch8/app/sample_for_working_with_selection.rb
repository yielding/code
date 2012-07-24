class SampleForWorkingWithSelection < UIViewController
  KEYBOARD_HEIGHT = 216.0

  def viewDidLoad
    super

    @tv = UITextView.new
    @tv.frame = self.view.bounds
    @tv.autoresizingMask = UIViewAutoresizingFlexibleWidth | 
      UIViewAutoresizingFlexibleHeight

    self.view.addSubview(@tv)

    has_text_btn = UIBarButtonItem.alloc.initWithTitle("hasText",
       style:UIBarButtonItemStyleBordered,
      target:self, 
      action:"hasTextDidPush")

    selection_btn = UIBarButtonItem.alloc.initWithTitle("selection",
       style:UIBarButtonItemStyleBordered,
      target:self, 
      action:"selectionDidPush")

    alignment_btn = UIBarButtonItem.alloc.initWithTitle("alignment",
       style:UIBarButtonItemStyleBordered,
      target:self, 
      action:"alignmentDidPush")

    scroll_btn = UIBarButtonItem.alloc.initWithTitle("top",
       style:UIBarButtonItemStyleBordered,
      target:self, 
      action:"scrollDidPush")

    buttons = [has_text_btn, selection_btn, alignment_btn, scroll_btn]
    self.setToolbarItems(buttons, animated:true)
  end

  def viewDidAppear animated
    super animated

    UIView.beginAnimations(nil, context:nil)
    UIView.setAnimationDuration(0.3)
    size = self.view.bounds.size
    @tv.frame = [[0, 0], [size.width, size.height - KEYBOARD_HEIGHT]]
    tb_frame = self.navigationController.toolbar.frame
    tb_frame.origin.y = view.window.bounds.size.height - tb_frame.size.height - KEYBOARD_HEIGHT
    self.navigationController.toolbar.frame = tb_frame
    UIView.commitAnimations
    @tv.becomeFirstResponder
  end

  def viewWillDisappear animated
    super animated
    UIView.beginAnimations(nil, context:nil)
    UIView.setAnimationDuration(0.3)

    @tv.frame = view.bounds
    tb_frame  = navigationController.toolbar.frame
    tb_frame.origin.y = view.window.bounds.size.height - tb_frame.size.height;
    navigationController.toolbar.frame = tb_frame
    UIView.commitAnimations
    @tv.resignFirstResponder
  end

  def hasTextDidPush
    alert = UIAlertView.new
    alert.message = @tv.hasText ? "@tv.hasText = true" : "@tv.hasText = false"
    alert.addButtonWithTitle "OK"
    alert.show
  end

  def selectionDidPush
    alert = UIAlertView.new
    loc = @tv.selectedRange.location
    len = @tv.selectedRange.length
    alert.message = "location: #{loc}, length: #{len}"
    alert.addButtonWithTitle "OK"
    alert.show
  end

  def alignmentDidPush
    @tv.editable = false
    @tv.textAlignment +=1
    @tv.textAlignment = UITextAlignmentLeft if @tv.textAlignment > UITextAlignmentRight
    @tv.editable = true
  end

  def scrollDidPush
    @tv.scrollRangeToVisible(NSMakeRange(0, 1))
  end
end
