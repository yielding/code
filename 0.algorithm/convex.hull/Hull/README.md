REMARK
======
  1. 아래처럼 썼다면 self.view 자체가 canvas view가 되는 것
     viewDidLoad 이하에 코드를 삽입하는 것은 기본 self.view의 자식에 다른 view를 삽입하는 것

     def loadView
       self.view = CanvasView.alloc.init
     end

  2. queue 객체 연습
     def viewDidLoad
       super
       #@queue = Dispatch::Queue.new("com.myhome.hull.task")
       ...
     end

     def clearCanvas
       # @queue.async { sleep 1; puts :hello }
     end
