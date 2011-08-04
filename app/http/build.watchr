watch('./*\.h') { |md| 
    system "rake" 
    system "./http_clinet"
}

watch('./*\.cpp') { |md| 
    system "rake" 
    system "./http_clinet"
}
