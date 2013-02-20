class RootViewController < UITableViewController
  def viewDidLoad
    super

    self.title = "Menu"
    @items = [
      "SampleForBlendMode",
      "SampleForOpaque",
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

    nc = navigationController
    nc.setNavigationBarHidden(false, animated:false)
    nc.setToolbarHidden(false, animated:false)
    nc.toolbar.barStyle    = UIBarStyleDefault
    nc.toolbar.translucent = false

    UIView.setAnimationsEnabled(true)
  end

  def tableView(tv, numberOfRowsInSection:section)
    @items.size 
  end

  CELLID = "Cell"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end

    cell.textLabel.text = @items[ip.row].gsub(/SampleFor/, '')
    cell
  end

  def tableView(tv, didSelectRowAtIndexPath:ip)
    vc = Object.const_get(@items[ip.row]).alloc.init 
    navigationController.pushViewController(vc, animated:true) unless vc.nil?
  end
end
