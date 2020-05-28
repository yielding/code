#!/usr/bin/env ruby

require "selenium-webdriver"

# configure the driver to run in headless mode
options = Selenium::WebDriver::Chrome::Options.new
#options.add_argument('--headless')

driver = Selenium::WebDriver.for(:chrome, options: options)

driver.navigate.to("https://www.google.com")

# resize the window and take a screenshot
driver.manage.window.resize_to(800, 800)
driver.save_screenshot("screenshot.png")

