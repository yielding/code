class SampleForBlackCellTable < SampleForSimpleTable
  def viewDidLoad
    super
    tableView.backgroundColor = UIColor.blackColor
    tableView.rowHeight = 128.0
    tableView.separatorStyle = UITableViewCellSeparatorStyleNone
  end

  CELLID = "basis-cell"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID)
    if cell.nil?
      cell = UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
      cell.textLabel.textColor = UIColor.whiteColor
      cell.textLabel.textAlignment = UITextAlignmentCenter
      cell.textLabel.font = UIFont.systemFontOfSize(64)
    end

    cell.textLabel.text = @data_source[ip.row]
    cell
  end
end
