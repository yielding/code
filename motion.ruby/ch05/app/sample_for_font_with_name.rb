class SampleForFontWithName < UITableViewController
  def init
    super.initWithStyle(UITableViewStyleGrouped)
    self
  end

  def numberOfSectionsInTableView(tv)
    fonts.count
  end

  def tableView(tv, numberOfRowsInSection:section)
    fonts[section][:fonts].count
  end

  CELLID = "Cell"
  def tableView(tableView, cellForRowAtIndexPath:ip)
    cell = tableView.dequeueReusableCellWithIdentifier(CELLID) || begin 
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end

    family_name = fonts[ip.section][:family]
    font_name   = fonts[ip.section][:fonts][ip.row]
    cell.textLabel.font = UIFont.fontWithName(font_name, size:UIFont.labelFontSize)
    cell.textLabel.text = font_name
    cell
  end

  def tableView(tv, titleForHeaderInSection:section)
    fonts[section][:family]
  end

  def fonts
     @fonts ||= begin
        UIFont.familyNames.sort.map do |family|
          fonts = UIFont.fontNamesForFamilyName(family).sort 
          { :family => family, :fonts => fonts }
        end
     end
  end
end
