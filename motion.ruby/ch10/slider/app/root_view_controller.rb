class RootViewController < UITableViewController
  def viewDidLoad
    super

    self.title = "메뉴"
    @items = ["SampleForSearchDisplay"]
  end

  def viewWillAppear(animated)
    super

    navigationController.setNavigationBarHidden(false, animated:false)
    navigationController.setToolbarHidden(false, animated:false)

    UIApplication.sharedApplication.statusBarStyle = UIStatusBarStyleDefault
    navigationController.navigationBar.barStyle    = UIBarStyleDefault
    navigationController.navigationBar.translucent = false
    navigationController.navigationBar.tintColor   = nil
    navigationController.toolbar.barStyle  = UIBarStyleDefault
    navigationController.toolbar.tintColor = nil
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

