class RootViewController < UITableViewController
  def viewDidLoad
    super
    self.title = "메뉴"
    @items = [
      "SampleForEditableTextView", 
      "SampleForTextView"
    ]
  end

  def viewWillAppear(animated)
    super animated
    navigationController.setNavigationBarHidden(false, animated:false)
    navigationController.setToolbarHidden(false, animated:false)

    UIApplication.sharedApplication.statusBarStyle = UIStatusBarStyleDefault
    navigationController.navigationBar.barStyle = UIBarStyleDefault
    navigationController.navigationBar.translucent = false
    navigationController.toolbar.barStyle = UIBarStyleDefault
    navigationController.toolbar.translucent = false

    UIView.setAnimationsEnabled(true)
  end

  def tableView(tv, numberOfRowsInSection: section)
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
