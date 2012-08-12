class SampleForSelectionStyle < UITableViewController
  def viewDidLoad
    super

    @ds = ["SelectionStyleBlue", "SelectionStyleGray", "SelectionStyleNone"]

    @ds_style = [
      UITableViewCellSelectionStyleBlue,
      UITableViewCellSelectionStyleGray,
      UITableViewCellSelectionStyleNone
    ]
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @ds.size
  end

  CELLID = "id"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin 
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end

    cell.textLabel.text = @ds[ip.row]
    cell.selectionStyle = @ds_style[ip.row]
    cell.textLabel.highlightedTextColor = UIColor.blackColor
    cell
  end
end
