#!/usr/bin/env ruby

require 'soap/wsdlDriver'
require 'rexml/document'

URL = 'http://www.webservicex.net/globalweather.asmx?WSDL'

# process the comandline arguments
if ARGV[0] == nil
  abort("Usage: weather.rb city")
else
  city = ARGV.join(' ')
end

soap = SOAP::WSDLDriverFactory.new(URL).create_rpc_driver
begin
  weather = soap.GetWeather(:CityName => city, :CountryName => "")

  # strip the first line with <? ?> stuff, else REXML wont parse
  xml  = weather.getWeatherResult.gsub(/(<\?.*?>\n)/, '')
  data = REXML::Document.new(xml)

  # celsius degrees are in parentheses
  data.elements["//Temperature"].text[/\((.*)\)/]; temp = $1
  data.elements["//Location"].text[/^(.*),/]; loc = $1

  # show the gathered data
  puts "The temperature in " + loc + " is " + temp
rescue
  puts "Could not find data for your supplied city: " + city
end
