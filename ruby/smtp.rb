#!/usr/bin/env ruby
# encoding: utf-8

require 'mail'

options = { :address              => "smtp.gmail.com",
            :port                 => 587,
            :domain               => 'saturn',
            :user_name            => 'yielding',
            :password             => 'Kamin1974',
            :authentication       => 'plain',
            :enable_starttls_auto => true  }



Mail.defaults do
  delivery_method :smtp, options
end

Mail.deliver do
       to 'yielding@gmail.com'
     from 'yielding@gmail.com'
  subject 'testing sendmail'
     body '우리나라 만세'
end
