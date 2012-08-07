class UITextView 
  attr_accessor :enablesReturnKeyAutomatically
end

class SampleForKeyBoard < UIViewController 
  KEYBOARD_HEIGHT = 216.0

  def viewDidLoad
    super

    @tv = UITextView.alloc.init 
    @tv.frame = self.view.bounds
    @tv.autoresizingMask = 
      UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight

    @tv.text = "이 텍스를 편집 가능합니다"
    self.view.addSubview(@tv)

    type_btn = UIBarButtonItem.alloc.initWithTitle("type",
                   style:UIBarButtonItemStyleBordered,
                  target:self,
                  action:"typeDidPush")

    return_type_btn = UIBarButtonItem.alloc.initWithTitle("return",
                   style:UIBarButtonItemStyleBordered,
                  target:self,
                  action:"returnTypeDidPush")

    capitalizatin_btn = UIBarButtonItem.alloc.initWithTitle("capital",
                   style:UIBarButtonItemStyleBordered,
                  target:self,
                  action:"capitalizatinDidPush")

    correction_btn = UIBarButtonItem.alloc.initWithTitle("correct",
                   style:UIBarButtonItemStyleBordered,
                  target:self,
                  action:"correctionDidPush")

    enable_return_btn = UIBarButtonItem.alloc.initWithTitle("enable",
                   style:UIBarButtonItemStyleBordered,
                  target:self,
                  action:"enablesReturnTypeDidPush")

    buttons = [type_btn, return_type_btn, capitalizatin_btn, correction_btn, enable_return_btn]

    self.setToolbarItems(buttons, animated:true)
  end

  def viewDidAppear animated
    super animated

    UIView.beginAnimations(nil, context:nil)
    UIView.setAnimationDuration(0.3)
    @tv.frame = [[0, 0], [self.view.bounds.size.width, self.view.bounds.size.height - KEYBOARD_HEIGHT]]

    toolbar_frame = self.navigationController.toolbar.frame
    toolbar_frame.origin.y = 
      self.view.window.bounds.size.height - toolbar_frame.size.height - KEYBOARD_HEIGHT
    self.navigationController.toolbar.frame = toolbar_frame
    UIView.commitAnimations
    @tv.becomeFirstResponder
  end

  def viewWillDisappear animated
    super animated

    UIView.beginAnimations(nil, context:nil)
    UIView.setAnimationDuration(0.3)
    @tv.frame = self.view.bounds
    toolbar_frame = self.navigationController.toolbar.frame
    toolbar_frame.origin.y = self.view.window.bounds.size.height - toolbar_frame.size.height
    self.navigationController.toolbar.frame = toolbar_frame
    UIView.commitAnimations
    @tv.resignFirstResponder
  end

  def typeDidPush
    @tv.editable = false
    @tv.keyboardType += 1
    @tv.keyboardType = UIKeyboardTypeDefault if @tv.keyboardType > UIKeyboardTypeEmailAddress

    msg = ""
    case @tv.keyboardType
    when UIKeyboardTypeDefault;      msg = "UIKeyboardTypeDefault"
    when UIKeyboardTypeASCIICapable; msg = "UIKeyboardTypeASCIICapable"
    when UIKeyboardTypeNumbersAndPunctuation; msg = "UIKeyboardTypeNumbersAndPunctuation"
    when UIKeyboardTypeURL;          msg = "UIKeyboardTypeURL"
    when UIKeyboardTypeNumberPad;    msg = "UIKeyboardTypeNumberPad"
    when UIKeyboardTypePhonePad;     msg = "UIKeyboardTypePhonePad"
    when UIKeyboardTypeNamePhonePad; msg = "UIKeyboardTypeNamePhonePad"
    when UIKeyboardTypeEmailAddress; msg = "UIKeyboardTypeEmailAddress"
    end

    @tv.text = msg
    @tv.editable = true
  end

  def returnTypeDidPush
    @tv.editable = false
    @tv.returnKeyType += 1
    @tv.returnKeyType = UIReturnKeyDefault if @tv.returnKeyType > UIReturnKeyEmergencyCall

    case @tv.returnKeyType
    when UIReturnKeyDefault;       @tv.text = "UIReturnKeyDefault"
    when UIReturnKeyGo;            @tv.text = "UIReturnKeyGo"
    when UIReturnKeyGoogle;        @tv.text = "UIReturnKeyGoogle"
    when UIReturnKeyJoin;          @tv.text = "UIReturnKeyJoin"
    when UIReturnKeyRoute;         @tv.text = "UIReturnKeyRoute"
    when UIReturnKeySearch;        @tv.text = "UIReturnKeySearch"
    when UIReturnKeySend;          @tv.text = "UIReturnKeySend"
    when UIReturnKeyYahoo;         @tv.text = "UIReturnKeyYahoo"
    when UIReturnKeyDone;          @tv.text = "UIReturnKeyDone"
    when UIReturnKeyEmergencyCall; @tv.text = "UIReturnKeyEmergencyCall"
    end

    @tv.editable = true;
  end

  def capitalizatinDidPush
    @tv.editable = false
    @tv.autocapitalizationType += 1
    if @tv.autocapitalizationType > UITextAutocapitalizationTypeAllCharacters
      @tv.autocapitalizationType = UITextAutocapitalizationTypeNone 
    end

    case @tv.autocapitalizationType
    when UITextAutocapitalizationTypeNone;          @tv.text = "UITextAutocapitalizationTypeNone"
    when UITextAutocapitalizationTypeWords;         @tv.text = "UITextAutocapitalizationTypeWords"
    when UITextAutocapitalizationTypeSentences;     @tv.text = "UITextAutocapitalizationTypeSentences"
    when UITextAutocapitalizationTypeAllCharacters; @tv.text = "UITextAutocapitalizationTypeAllCharacters"
    end
    @tv.editable = true;

  end

  def correctionDidPush
    @tv.editable = false;
    @tv.autocorrectionType += 1
    @tv.autocorrectionType = UITextAutocorrectionTypeDefault if @tv.autocorrectionType > UITextAutocorrectionTypeYes

    case @tv.autocorrectionType
    when UITextAutocorrectionTypeDefault; @tv.text = "UITextAutocorrectionTypeDefault"
    when UITextAutocorrectionTypeNo;      @tv.text = "UITextAutocorrectionTypeNo"
    when UITextAutocorrectionTypeYes;     @tv.text = "UITextAutocorrectionTypeYes"
    end
    @tv.editable = true;
    
  end

  def enablesReturnTypeDidPush
    @tv.editable = false;
    @tv.enablesReturnKeyAutomatically = !@tv.enablesReturnKeyAutomatically

    # TODO
    # I think below below message has some problem!!!!!
    puts "automatic? #{@tv.enablesReturnKeyAutomatically}"

    @tv.text = if @tv.enablesReturnKeyAutomatically
                  "enablesReturnKeyAutomatically = true"
               else
                  "enablesReturnKeyAutomatically = false"
               end

    @tv.editable = true
  end

end
