class CellWithImageView < UITableViewCell
  def initWithReuseIdentifier(id)
  end

  def layoutSubviews
  end
end

class CellWithSwitch < UITableViewCell
  def initWithReuseIdentifier(id)
  end

  def layoutSubviews
  end

end

class CellWithSlider < UITableViewCell
  def initWithReuseIdentifier(id)
  end

  def layoutSubviews
  end

  def sliderValueDidChange(slider)
  end

end

class SampleForCustomizedCell < UITableViewController
  def init
    self.initWithStyle(UITableViewStyleGrouped)
    self
  end

  def viewDidLoad
    super
  end

  def numberOfSectionsInTableView(tv)
  end

  def tableView(tv, numberOfSectionsInTableView:sec)
  end

  def tableView(tv, cellForRowAtIndexPath:ip)
  end

  def tableViewe(tv, heightForRowAtIndexPath:ip)
  end

end
