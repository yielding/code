class SampleForScroll < SampleForSimpleTable
  def viewDidLoad
    super

    top_btn = UIBarButtonItem.alloc.initWithTitle("top",
                style:UIBarButtonItemStyleBordered,
                target:self,
                action: :"topDidPush")

    cur_btn = UIBarButtonItem.alloc.initWithTitle("current",
                style:UIBarButtonItemStyleBordered,
                target:self,
                action: :"currentDidPush")

    bot_btn = UIBarButtonItem.alloc.initWithTitle("bottom",
                style:UIBarButtonItemStyleBordered,
                target:self,
                action: :"bottomDidPush")

    self.setToolbarItems([top_btn, cur_btn, bot_btn], animated:true)
  end

  def topDidPush
    ip = NSIndexPath.indexPathForRow(0, inSection:0)
    self.tableView.scrollToRowAtIndexPath(ip,
            atScrollPosition:UITableViewScrollPositionNone,
            animated:true)
  end

  def currentDidPush
    self.tableView.scrollToNearestSelectedRowAtScrollPosition(
            UITableViewScrollPositionNone,
            animated:true)
  end

  def bottomDidPush
    ip = NSIndexPath.indexPathForRow(@data_source.size-1, inSection:0)
    self.tableView.scrollToRowAtIndexPath(ip,
            atScrollPosition:UITableViewScrollPositionNone,
            animated:true)
  end

  def tableView(tv, didSelectRowAtIndexPath:ip)
  end

end
