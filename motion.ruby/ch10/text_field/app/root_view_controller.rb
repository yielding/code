class RootViewController < UITableViewController
  def viewDidLoad
    super
    title = "메뉴"
    @items ||= [ 
      "SampleForChangeColorAndFont",
      "SampleForBackground",
      "SampleForAddView",
      "SampleForObserving",
      "SampleForAdjustFontSize",
      "SampleForClearButtonMode",
      "SampleForPlaceholder",
      "SampleForSimple"
    ]
  end

  def viewWillAppear animated
    super
    navigationController.setNavigationBarHidden(false, animated:false)
    navigationController.setToolbarHidden(false, animated:false)

    UIApplication.sharedApplication.statusBarStyle = UIStatusBarStyleDefault
    navigationController.navigationBar.barStyle    = UIBarStyleDefault
    navigationController.navigationBar.translucent = false
    navigationController.navigationBar.tintColor   = nil
    navigationController.toolbar.barStyle          = UIBarStyleDefault
    navigationController.toolbar.translucent       = false
    navigationController.toolbar.tintColor         = nil
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @items.size
  end

  CELL_ID = "Cell"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELL_ID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELL_ID)
    end

    cell.textLabel.text = @items[ip.row].gsub(/SampleFor/, '')
    cell
  end
  
  def tableView(tv, didSelectRowAtIndexPath:ip)
    vc = Object.const_get(@items[ip.row]).new  
    navigationController.pushViewController(vc, animated:true) unless vc.nil?
  end
end
