class ConvexHullViewController < UIViewController
  def viewDidLoad
    super

    self.title = "Jarvis March"
    vs = view.bounds.size
    ns = navigationController.navigationBar.frame.size
    ts = navigationController.toolbar.frame.size
    frame = [[0, 0], [vs.width, vs.height]]
    @canvas_view = CanvasView.alloc.initWithFrame(frame)
    @canvas_view.selected_mode = 0

    self.view.addSubview(@canvas_view)
  end

  def viewWillAppear(animated)
    super(animated)

    navigationController.setNavigationBarHidden(false, animated:true)
    navigationController.setToolbarHidden(false, animated:true)

    UIApplication.sharedApplication.statusBarStyle = UIStatusBarStyleDefault
    
    nb = self.navigationController.navigationBar 
    nb.barStyle    = UIBarStyleDefault
    nb.translucent = true; nb.alpha = 0.8

    tb = self.navigationController.toolbar
    tb.barStyle    = UIBarStyleDefault
    tb.translucent = true; tb.alpha = 0.8

    b1 = barButtonItem(UIBarButtonSystemItemRefresh, 'clearCanvas')
    b2 = barButtonItem(UIBarButtonSystemItemPlay,    'findHull')
    sp = barButtonItem(UIBarButtonSystemItemFlexibleSpace, nil)

    sg = UISegmentedControl.alloc.initWithItems(["1", "3", "5"])
    sg.selectedSegmentIndex = 0
    sg.frame = [[0, 0], [100, 30]]
    sg.addTarget(self, action:'segmentDidChange:', 
                       forControlEvents:UIControlEventValueChanged)
    bs = UIBarButtonItem.alloc.initWithCustomView(sg)

    #self.setToolbarItems([b1, b2, sp, bs], animated:true);
    self.setToolbarItems([b1, b2, bs], animated:true);

    UIView.setAnimationsEnabled(true)
  end

  def barButtonItem(item, action)
    UIBarButtonItem.alloc.initWithBarButtonSystemItem(item, target:self, action: action)
  end

  def clearCanvas
    @canvas_view.clearCanvas
  end

  def findHull
    @canvas_view.findHull
  end

  def segmentDidChange sender 
    @canvas_view.selected_mode = sender.selectedSegmentIndex
  end
end
