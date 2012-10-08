class SampleForLinks < UITableViewController
  def viewDidLoad
    super
    @ds = [ 
      "Web Links", "Mail Links", "Phone Links", "Text Links",
      "Map Links", "YouTube Links", "iTunes Links", "Customize Links"
    ]

    @urls = [
      "http://www.apple.com/",
      "mailto:ITsAndroid.jp@gmail.com?subject=hello&body=hello%20mail!",
      "tel:090-0000-0000",
      "sms:090-0000-0000",
      "http://maps.google.com/maps?ll=37.550532%2C126.988102&z=15",
      "http://youtu.be/0kCnvfXLLlE",
      "http://itunes.apple.com/kr/app/id352021130?mt=8",
      "sampleapp://sampleapp.yourcompany.com/?Hell0"
    ]
      
  end

  def tableView(tv, numberOfRowsInSection:sec)
    @ds.size
  end

  CELLID = "basis-cell"
  def tableView(tv, cellForRowAtIndexPath:ip)
    cell = tv.dequeueReusableCellWithIdentifier(CELLID)
    if cell.nil?
      cell = UITableViewCell.alloc.initWithStyle(UITableViewCellStyleDefault,
                                                 reuseIdentifier:CELLID)
    end
    cell.textLabel.text = @ds[ip.row]
    cell
  end

  def tableView(tv, didSelectRowAtIndexPath:ip)
    app = UIApplication.sharedApplication
    url = NSURL.URLWithString(@urls[ip.row])
    app.openURL(url) if app.canOpenURL(url)
  end

end
