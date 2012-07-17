class SampleForAlert < UIViewController
  def viewDidAppear(animated)
    super(animated)
    alert = UIAlertView.alloc.init 
    alert.title   = "유감스러운 소식"
    alert.message = "당신의 저장 데이터가 사라져 버렸습니다"
    alert.addButtonWithTitle("OK")
    alert.show
  end
end
