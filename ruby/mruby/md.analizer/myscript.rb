fss = $ds.file_systems
puts fss
fss.each { |fs| puts fs.name }
