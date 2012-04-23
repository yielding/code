#!/usr/local/bin/macruby

framework 'Cocoa'

class DownloadDelegator
  def downloadDidBegin(dl_process) 
    puts "downloading..."
  end
  
  def download(dl_process, decideDestinationWithSuggestedFilename:filename) 
    home = NSHomeDirectory()
    path = home.stringByAppendingPathComponent('Desktop')
    path = path.stringByAppendingPathComponent(filename) 
    dl_process.setDestination(path, allowOverwrite:true)
  end
  
  def download(dl_process, didFailWithError:error)
    error_description = error.localizedDescription
    more_details = error.userInfo[NSErrorFailingURLStringKey]
    puts "Download failed. #{error_description} - #{more_details}"
    exit 
  end
  
  def downloadDidFinish(dl_process) 
    puts "Download finished!"
    exit
  end 
end

url_string = 'http://www.macruby.org/files/nightlies/macruby_nightly-latest.pkg' 
url = NSURL.URLWithString(url_string)
req = NSURLRequest.requestWithURL(url)
file_download = NSURLDownload.alloc.initWithRequest(req, delegate: DownloadDelegator.new)
# keep the run loop running 
NSRunLoop.currentRunLoop.runUntilDate(NSDate.distantFuture)