class ConvexHullViewController < UIViewController
=begin rdoc
  # 아래처럼 썼다면 self.view 자체가 canvas view가 되는 것
  # viewDidLoad 이하에 코드를 삽입하는 것은 기본 self.view의 자식에 다른 view
  # 를 삽입하는 것
  def loadView
    self.view = CanvasView.alloc.init
  end
=end

  def viewDidLoad
    super

    self.title = "Jarvis March"

    vs = view.bounds.size
    ns = navigationController.navigationBar.frame.size
    ts = navigationController.toolbar.frame.size
    frame = [[0, ts.height], [vs.width, vs.height - ns.height - ns.height]]
    @canvas_view = CanvasView.alloc.initWithFrame(frame)

    self.view.addSubview(@canvas_view)
  end

  def viewWillAppear(animated)
    super(animated)

    navigationController.setNavigationBarHidden(false, animated:true)
    navigationController.setToolbarHidden(false, animated:true)

    UIApplication.sharedApplication.statusBarStyle = UIStatusBarStyleDefault
    nb = self.navigationController.navigationBar 
    nb.barStyle    = UIBarStyleDefault
    nb.translucent = true

    tb = self.navigationController.toolbar
    tb.barStyle    = UIBarStyleDefault
    tb.translucent = true
    b1 = UIBarButtonItem.alloc.initWithBarButtonSystemItem(
            UIBarButtonSystemItemRefresh,
            target:self,
            action:'clearCanvas')

    b2 = UIBarButtonItem.alloc.initWithBarButtonSystemItem(
            UIBarButtonSystemItemAction,
            target:self,
            action:'findHull')

    self.setToolbarItems([b1, b2], animated:true);

    UIView.setAnimationsEnabled(true)
  end

  def clearCanvas
    @canvas_view.clearCanvas
  end

  def findHull
    @canvas_view.findHull
  end
end
