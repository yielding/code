class SampleForAlertWithMultiButtons < UIViewController
  def viewDidAppear(animated)
    a = UIAlertView.alloc.init 
    a.delegate = self
    a.title = "저장 데이터 복구"
    a.message = "your data have been lost. Are you going to go back?"
    a.addButtonWithTitle "No"
    a.addButtonWithTitle "Yes"
    a.cancelButtonIndex = 0
    a.show
  end

  def alertView(av, clickedButtonAtIndex:index)
    return unless index == av.cancelButtonIndex

    a = UIAlertView.alloc.init 
    a.title = "복구 실패"
    a.message = "유감스럽지만 복구를 실패했어요"
    a.addButtonWithTitle "OK" 
    a.show
  end

end
