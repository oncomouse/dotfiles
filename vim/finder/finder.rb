# frozen_string_literal: true

require 'json'
require 'optparse'
require 'open-uri'

options = {}
OptionParser.new do |opts|
  opts.banner = 'Usage: example.rb [options]'

  opts.on('-p',
          '--package PACKAGE',
          String,
          %i[ale-coc.nvim coc.nvim denite clap],
          'Which package to load (ale-coc.nvim, coc.nvim, denite, clap)') do |package|
    options[:package] = package.to_s
  end
end.parse!

def url_to_file(package, url)
  "#{ENV['HOME']}/dotfiles#{url.split(package)[1..].join(package)}"
end

manifest = JSON.parse(File.read('./manifest.json'))
if options[:package].nil?
  puts "Missing --package option. Values can be #{manifest.keys.join(', ')}."
  exit 1
end
local_files = manifest[options[:package]]['files'].map { |url| url_to_file(options[:package], url) }
files_to_delete = manifest
                  .keys
                  .filter { |key| key != options[:package] }
                  .map do |package|
  manifest[package]['files']
    .map { |url| url_to_file(package, url) }
    .filter { |file| File.exist?(file) && !local_files.include?(file) }
end
                  .flatten
                  .uniq
# Remove unwanted files
puts files_to_delete
File.unlink(*files_to_delete)

manifest[options[:package]]['files'].each do |url|
  contents = URI.open(url).read
  file = url_to_file(options[:package], url)
  File.open(file, 'w') do |fp|
    fp.puts(contents)
  end
end
