class SampleForCellWithAccessory < SampleForCellWithDetail
  def viewDidLoad
    super
    @accessory_type = UITableViewCellAccessoryNone
    btn = UIBarButtonItem.alloc.initWithTitle("accesory type",
            style:UIBarButtonItemStyleBordered,
            target:self,
            action:"buttonDidPush")

    self.navigationItem.rightBarButtonItem = btn
  end

  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = super
    cell.accessoryType = @accessory_type
    cell
  end

  def buttonDidPush
    @accessory_type += 1
    if @accessory_type > UITableViewCellAccessoryCheckmark
       @accessory_type = UITableViewCellAccessoryNone 
    end

    self.tableView.reloadData
  end

  def tableView(tv, accessoryButtonTappedForRowWithIndexPath:ip)
    vc = SampleForCellWithDetail.alloc.init
    self.navigationController.pushViewController(vc, animated:true)
  end
end
