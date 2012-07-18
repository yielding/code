class SampleForAlertWithLongMessage < UIViewController 
  def viewDidAppear(animated)
    super(animated)

    alert = UIAlertView.alloc.init 
    alert.title = "long message"
    alert.message = "1st\n2nd\n3rd\n4th\n5th\n6th\7th"
    alert.addButtonWithTitle("OK")
    alert.show
  end
      
end
