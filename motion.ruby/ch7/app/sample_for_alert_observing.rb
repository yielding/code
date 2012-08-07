class SampleForAlertObserving < UIViewController
  def viewDidAppear(animated)
    super(animated)

    alert = UIAlertView.alloc.init 
    alert.delegate = self 
    alert.message  = "delegate 메소드 테스트..."
    alert.addButtonWithTitle "Cancel"
    alert.addButtonWithTitle "OK"
    alert.cancelButtonIndex = 0
    alert.show
  end

  def alertView(av, clickedButtonAtIndex:index)
    msg = (av.cancelButtonIndex == index) ? "Cancel" : "OK"
    puts msg 
  end

  def willPresentAlertView(av)
    puts "willPresentAlertView"
  end

  def didPresentAlertView(av)
    puts "didPresentAlertView"
  end

  def alertView(av, willDismissWithButtonIndex:index)
    puts "willDismissWithButtonIndex: #{index}"
  end

  def alertView(av, didDismissWithButtonIndex:index)
    puts "didDismissWithButtonIndex"
  end

  def alertViewCancel(av)
    puts "Cancel"
  end

  def shouldAutorotateToInterfaceOrientation(ori)
    true
  end
end
