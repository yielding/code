#!/usr/bin/env ruby2.0

module NumberQuery
  refine String do
    def number?
      match(/^[1-9][0-9]+$/) ? true : false
    end
  end
end

module NumberQuery
  p "123".number?   #=> true
end

module MyApp
  using NumberQuery

  p "123".number?   #=> true
end

