class MainViewController < UIViewController
  def viewDidLoad
    super

    paint_view = PaintView.alloc.initWithFrame(self.view.bounds)
    view.addSubview(paint_view)
  end
end
