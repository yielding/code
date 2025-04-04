#!/bin/bash

bundle exec rspec
if [ $? -eq 0 ]; then
  terminal-notifier -title "RSpec" \
    -message "✅ All tests passed!" \
    -sender com.apple.Terminal
else
  terminal-notifier -title "RSpec" \
    -message "❌ Tests failed!" \
    -sender com.apple.Terminal
fi