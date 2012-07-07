class RootViewController < UITableViewController
  def viewDidLoad
    super
    self.title = "Menu"
    @items = [
      "SampleForTransition",
      "SampleForAnimationCurve",
      "SampleForUIImageViewAnimation", 
      "SampleForTextAlignment",
      "SampleForLabel",
      "SampleForDrawStrings",
      "SampleForDrawStringsInRect",
      "SampleForBaselineAdjustment",
      "SampleForMinFontSize",
      "SampleForSizeWithFont",
      "SampleForFontWithName",
      "SampleForUIImage",
      "SampleForUIImageView"
    ]
  end

  def viewWillAppear(animated)
    super(animated)

    self.navigationController.setNavigationBarHidden(false, animated:false)
    self.navigationController.setToolbarHidden(false, animated:false)
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
    self.navigationController.pushViewController(vc, animated:true) unless vc.nil?
  end
end
