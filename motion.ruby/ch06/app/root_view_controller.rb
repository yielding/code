class RootViewController < UITableViewController
  def viewDidLoad
    super
    self.title = "Menu"
    @items = [
      "SampleForAccelerometer", 
      "SampleForPinch",
      "SampleForMotion",
      "SampleForSlide",
      "SampleForTripleTap",
      "SampleForDoubleTap",
      "SampleForDrag",
      "SampleForResponderChain",
      "SampleForTouchesLabel",
      "SampleForTouchesBegin",
      "SampleForSlider",
      "SampleForButton"
    ]
  end

  def viewWillAppear(animated)
    super(animated)
    navigationController.setNavigationBarHidden(false, animated:false)
    navigationController.setToolbarHidden(false, animated:false)

    UIApplication.sharedApplication.statusBarStyle = UIStatusBarStyleDefault
    self.navigationController.navigationBar.barStyle = UIBarStyleDefault
    self.navigationController.navigationBar.translucent = false
    self.navigationController.toolbar.barStyle = UIBarStyleDefault
    self.navigationController.toolbar.translucent = false

    UIView.setAnimationsEnabled(true)
  end

  def tableView(tv, numberOfRowsInSection:section)
    @items.size
  end

  CELLID = "Cell"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin 
      cell = UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
      cell
    end
    cell.textLabel.text = @items[ip.row].gsub(/SampleFor/, '')
    cell
  end

  def tableView(tv, didSelectRowAtIndexPath:ip)
   vc = Object.const_get(@items[ip.row]).alloc.init 
   navigationController.pushViewController(vc, animated:true) unless vc.nil?
  end

end
