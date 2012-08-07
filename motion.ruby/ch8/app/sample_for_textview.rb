class SampleForTextView < UIViewController
  def viewDidLoad
    super
    
    tv = UITextView.alloc.init
    tv.frame = self.view.bounds
    tv.autoresizingMask = UIViewAutoresizingFlexibleWidth or UIViewAutoresizingFlexibleHeight
    tv.editable = false
    tv.backgroundColor = UIColor.blackColor
    tv.textColor = UIColor.whiteColor
    tv.font = UIFont.systemFontOfSize 32
    tv.text = ["Hello, UITextView!", 
               "2nd", "3rd", "4th", "5th", 
               "6th", "7th", "8th"].join("\n")
    self.view.addSubview(tv)
  end
end
