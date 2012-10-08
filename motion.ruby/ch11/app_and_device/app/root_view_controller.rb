class RootViewController < UITableViewController
  def viewDidLoad
    super

    self.title = "메뉴"
    @items = %w{
      SampleForCopyAndPaste
      SampleForLinks
      SampleForBatteryMonitor
      SampleForDeviceInfo
      SampleForBadge
    }

  end

  def viewWillAppear animated
    super

    self.navigationController.setNavigationBarHidden(false, animated:false)
    self.navigationController.setToolbarHidden(false, animated:false)

    UIApplication.sharedApplication.statusBarStyle   = UIStatusBarStyleDefault
    self.navigationController.navigationBar.barStyle = UIBarStyleDefault
    self.navigationController.navigationBar.translucent = false
    self.navigationController.toolbar.barStyle = UIBarStyleDefault
    self.navigationController.toolbar.translucent = false

    UIView.setAnimationsEnabled(true)
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @items.size
  end

  CELLID="Cell"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end
    
    cell.textLabel.text = @items[ip.row].gsub(/SampleFor/, '')
    cell
  end

  def tableView(tv, didSelectRowAtIndexPath:ip)
    vc = Object.const_get(@items[ip.row]).new
    navigationController.pushViewController(vc, animated:true) unless vc.nil?
  end
end
