class SampleForCellWithImage < UITableViewController
  def viewDidLoad
    super

    @ds = ["Monkey", "Dog", "Lion", "Elephant"]
    @images = []

    @ds.each { |e| @images << UIImage.imageNamed("#{e}.png") }
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @ds.size
  end

  CELLID = "basis-cell"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID) || begin 
      UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault, reuseIdentifier:CELLID)
    end

    cell.textLabel.text  = @ds[ip.row]
    cell.imageView.image = @images[ip.row]
    cell
  end

end
