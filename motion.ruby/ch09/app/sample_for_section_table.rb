class SampleForSectionTable < UITableViewController
  def init
    self.title = "Section Table" if super
    self
  end

  def viewDidLoad
    super

    @data_source = { 
      "포유류" => ["Monkey", "Dog", "Lion", "Leopard"],
      "파충류" => ["Snake", "Geoko"],
      "양서류" => ["Frog", "Newts"],
      "어  류" => ["Shark", "Salmon"] }
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @data_source.to_a[sec][1].size
  end

  CELLID = "id"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end

    key  = @data_source.to_a[ip.section][0]
    text = @data_source[key][ip.row]
    cell.textLabel.text = text
    cell
  end

  def numberOfSectionsInTableView tv 
    @data_source.size
  end

  def tableView tv, titleForHeaderInSection:sec 
    @data_source.to_a[sec][0]
  end

  def sectionIndexTitlesForTableView tv 
    @data_source.keys
  end
end
