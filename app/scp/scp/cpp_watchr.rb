system 'clear'

def growl(message)
  growlnotify = `which growlnotify`.chomp
  title  = "Watchr Test Results"
  passed = message.include?('PASSED')
  image  = passed ? "~/.watchr_images/passed.png" : "~/.watchr_images/failed.png"
  severity = passed ? "-1" : "1"
  options = "-w -n Watchr --image '#{File.expand_path(image)}'"
  options << " -m '#{message}' '#{title}' -p #{severity}"

  system %(#{growlnotify} #{options} &)
end

def run(cmd)
  puts(cmd)
  `#{cmd}`
end

def executed_within sec
  now = Time.now
  if $last_executed.nil?
    $last_executed = now
    return true
  end

  res = now - $last_executed < sec
  $last_executed = now
  res;
end

def run_test_file(cmd)
  system('clear')
  build_result = run("rake test")
  if build_result.include?("build ok")
    result = run(cmd)
    growl result.split("\n").last rescue nil
    puts result
  else
    result = "build failed"
    growl(result)
    puts result
  end
end

watch ('develop/src/*\.cpp') { |m| 
  run_test_file('./test_scp') 
}

watch ('develop/include/*\.h') { |m| 
  run_test_file('./test_scp') 
}

watch('./*\.h') { |m| 
  run_test_file('./test_scp')
}

watch('./*\.cpp') { |m| 
  run_test_file('./test_scp')
}

watch('./Rakefile') { |m| 
  run_test_file('./test_scp')
}

# Ctrl-\
Signal.trap 'QUIT' do
  puts " --- Running all tests ---\n\n"
  run("rake clobber")
  `sleep 2`
end

@interrupted = false

# Ctrl-C
Signal.trap 'INT' do
  if @interrupted then
    @wants_to_quit = true
    abort("\n")
  else
    puts "Interrupt a second time to quit"
    @interrupted = true
    Kernel.sleep 1.5
    # raise Interrupt, nil # let the run loop catch it
    # run_suite
  end
end
