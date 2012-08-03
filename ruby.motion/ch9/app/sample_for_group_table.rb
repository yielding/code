class SampleForGroupTable < SampleForSectionTable
  def init
    super
    self.title = "GroupTable"
    self.initWithStyle(UITableViewStyleGrouped)
    self
  end
end
