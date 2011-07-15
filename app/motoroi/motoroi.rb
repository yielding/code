#!/usr/bin/env ruby 

require "net/http"
require "json"
require "pp"

class Picture
  attr_reader :name, :loc

  def initialize pic 
    @id,  @name = pic["id"], pic["PicName"]
    @loc, @tloc = pic["LOC"], pic["TLOC"]
    @size       = pic["Size"]
    @last_modified  = pic["LastModified"]
    @album_name     = pic["AlbumName"]
    @width, @height = pic["Width"], pic["Height"]
  end

  def assign contents
    @contents = contents
  end

  def save_to path
      File.open("#{path}/#{@name}", "w") { |file| file.write(@contents) }
  end

  def to_s
    "#{@name} : #{@loc}"
  end
end

class SMS
  def initialize(sms)
    @msg_type = sms["MsgType"]
    @id   = sms["MsgId"]
    @name = sms["Name"]
    @address = sms["Address"]
    @phone_type = sms["PhoneType"]
    @dir  = sms["DirId"]
    @ts   = sms["TimeStamp"]
    @duration = sms["CallDuration"]
    @read_state = sms["MsgReadState"]
    @body = sms["MsgBody"]
  end

  def to_s
    "#{@address}: #{@name} : #{@body}"
  end
end

class Motoroi
  def initialize ip, port=8080 
    @http = Net::HTTP::new(ip, port)
    @pictures = []
    @messages = []
  end

  def prepare_pictures
    # 1. prepare picture list
    _, data = @http.get("/personalportal/pictureview?c=0&blk=1")

    # 2. get total block count
    res = JSON.parse(data)
    total_blocks = res["TotalBlocks"]

    # 3. assemble the list we got
    res["RESPONSE"].each { |pic| @pictures << Picture.new(pic) } 

    # 4. get remaining picture list
    if total_blocks > 1
      2.upto(total_blocks) do |no|
        _, data = @http.get("/personalportal/pictureview?c=0&blk=#{no}")
        JSON.parse(data)["RESPONSE"].each { |pic| @pictures << Picture.new(pic) } 
      end
    end

    # 5. get actual files
    @pictures.each { |picture| 
      _, contents = @http.get(picture.loc)
      picture.assign contents
      p picture
    }
  end

  def prepare_messages
    # 1. prepare picture list
    _, data = @http.get("/personalportal/mobileinbox?c=0&d=4&blk=1")

    # 2. get total block count
    res = JSON.parse(data)["LoadMobileInboxResp"]
    total_blocks = res["TotalBlocks"]

    # 3. assemble the list we got
    res["MsgDesc"].each { |sms| @messages << SMS.new(sms) }

    # 4. get remaining message list
    if total_blocks > 1
      2.upto(total_blocks) do |no|
        _, data = @http.get("/personalportal/mobileinbox?c=0&d=4&blk=#{no}")
        res = JSON.parse(data)["LoadMobileInboxResp"]
        res["MsgDesc"].each { |sms| @messages << SMS.new(sms) } 
      end
    end

    @messages
  end
end

if __FILE__ == $PROGRAM_NAME
  motoroi  = Motoroi.new("192.168.1.7")
  mdf_path = "."
  motoroi.prepare_pictures.each { |picture| picture.save_to(mdf_path) }
  motoroi.prepare_messages.each { |message| p message }
end
