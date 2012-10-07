class SampleForDeviceInfo < UITableViewController
  def viewDidLoad
    super
    @titles = %w{ localizedModel model name systemName systemVersion }
    device  = UIDevice.currentDevice
    @data   = [
      device.localizedModel,
      device.model,
      device.name,
      device.systemName,
      device.systemVersion
    ]
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @titles.size
  end

  CELLID = "basis"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID)
    if cell.nil?
      cell = UITableViewCell.alloc.initWithStyle(UITableViewCellStyleValue1, 
                                                 reuseIdentifier:CELLID)
    end

    cell.textLabel.text       = @titles[ip.row]
    cell.detailTextLabel.text = @data[ip.row]
    cell
  end
end
