class RootViewController < UITableViewController
  def viewDidLoad
    super
    @title = "MENU"
    @items = [
      "SampleForFrame",
      "SampleForAlpha",
      "SampleForHide",
      "SampleForBackground",
      "SampleForTransform"
    ]
  end

  def viewWillAppear(animated)
    super animated
    navigationController.setNavigationBarHidden(true, animated:true)
  end

  def numberOfSectionsInTableView(tableView)
    1
  end

  def tableView(tv, numberOfRowsInSection: section)
    @items.size
  end

  CELLID = "Cell"
  def tableView(tv, cellForRowAtIndexPath: ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end

    cell.textLabel.text = @items[ip.row].gsub(/SampleFot/, '')
    cell
  end

  def tableView(tv, didSelectRowAtIndexPath: ip)
    vc = Object.const_get(@items[ip.row]).alloc.init
    navigationController.pushViewController(vc, animated:true) unless vc.nil?
  end
end
