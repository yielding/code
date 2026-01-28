#!/usr/bin/env ruby
# frozen_string_literal: true

################################################################################
#
# Ruby gRPC Client for ImageProcessor Service
#
################################################################################

$LOAD_PATH.unshift(File.join(__dir__, 'ruby_gen'))

require 'grpc'
require 'image_processor_services_pb'

module ImageProcessorClient
  class Client
    def initialize(server_address = 'localhost:50051')
      @stub = ImageProcessor::ImageProcessor::Stub.new(
        server_address,
        :this_channel_is_insecure
      )
    end

    def apply_gaussian_blur(input_path, output_path, kernel_size: 9, sigma: 2.0)
      # Read input image
      image_data = File.binread(input_path)
      puts "Sending image: #{image_data.bytesize} bytes"

      # Build request
      request = ImageProcessor::ImageRequest.new(
        image_data: image_data,
        kernel_size: kernel_size,
        sigma: sigma
      )

      # Call RPC
      response = @stub.apply_gaussian_blur(request)

      unless response.success
        warn "Processing failed: #{response.error_message}"
        return false
      end

      # Write output image
      File.binwrite(output_path, response.image_data)
      puts "Received image: #{response.image_data.bytesize} bytes"
      puts "Saved to: #{output_path}"

      true
    rescue GRPC::BadStatus => e
      warn "RPC failed: #{e.message}"
      false
    end
  end
end

################################################################################
#
# Main
#
################################################################################
def print_usage(program)
  warn "Usage: #{program} <input_image> <output_image> [server_address] [kernel_size] [sigma]"
  warn "  input_image:    Path to input image file"
  warn "  output_image:   Path to save processed image"
  warn "  server_address: gRPC server address (default: localhost:50051)"
  warn "  kernel_size:    Gaussian kernel size (default: 9)"
  warn "  sigma:          Gaussian sigma (default: 2.0)"
end

if __FILE__ == $PROGRAM_NAME
  if ARGV.length < 2
    print_usage($PROGRAM_NAME)
    exit 1
  end

  input_path     = ARGV[0]
  output_path    = ARGV[1]
  server_address = ARGV[2] || 'localhost:50051'
  kernel_size    = (ARGV[3] || 9).to_i
  sigma          = (ARGV[4] || 2.0).to_f

  unless File.exist?(input_path)
    warn "Input file not found: #{input_path}"
    exit 1
  end

  puts "Connecting to #{server_address}..."

  client = ImageProcessorClient::Client.new(server_address)
  success = client.apply_gaussian_blur(
    input_path,
    output_path,
    kernel_size: kernel_size,
    sigma: sigma
  )

  if success
    puts 'Image processing completed successfully'
    exit 0
  else
    warn 'Image processing failed'
    exit 1
  end
end
