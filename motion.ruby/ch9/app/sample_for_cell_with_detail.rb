class SampleForCellWithDetail < UITableViewController

  CELL_ID = { UITableViewCellStyleValue1   => "style-value1" ,
              UITableViewCellStyleValue2   => "style-value2", 
              UITableViewCellStyleSubtitle => "style-subtitle" }
  CELL_ID.default = "style-default"

  def viewDidLoad
    super
    @ds      = ["Monkey", "Dog", "Lion", "Elephant"]
    @images  = []
    @ds.each { |e| @images << UIImage.imageNamed("#{e}.png") }
    @details = ["원숭이", "강아지", "사자", "코끼리"]
    @cell_style = UITableViewCellStyleDefault

    btn = UIBarButtonItem.alloc.initWithTitle("CellStyle", 
            style:UIBarButtonItemStyleBordered,
            target:self,
            action:"buttonDidPush")

    self.navigationItem.rightBarButtonItem = btn
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @details.size
  end

  def tableView(tv, cellForRowAtIndexPath:ip)
    id = CELL_ID[@cell_style]
    cell = tv.dequeueReusableCellWithIdentifier(id) || begin
      UITableViewCell.alloc.initWithStyle(@cell_style, reuseIdentifier:id)
    end

    cell.textLabel.text = @ds[ip.row]
    cell.imageView.image = @images[ip.row]       unless @cell_style == UITableViewCellStyleValue2
    cell.detailTextLabel.text = @details[ip.row] unless @cell_style == UITableViewCellStyleDefault
    cell
  end

  def buttonDidPush
    @cell_style += 1
    if @cell_style > UITableViewCellStyleSubtitle
       @cell_style = UITableViewCellStyleDefault 
    end

    self.tableView.reloadData
  end
end
