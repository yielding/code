watch('./*\.cpp') { |md| 
    system "rake" 
    system "./container_device"
}
