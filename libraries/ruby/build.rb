#!/usr/bin/ruby

require 'fileutils'
require 'rubygems'

gemspec = Gem::Specification.new do |s|
  s.name = 'kropotkin'
  s.version = '0.0.1'
  s.summary = 'Client for the Kropotkin framework'
  s.description = 'Provides methods for creating Kropotkin factspaces, ' \
                + 'storing facts in them, and querying facts from them.'
  s.files = ["kropotkin-ruby.rb"]
  s.homepage = 'https://github.com/douglassquirrel/kropotkin'
  s.author = 'Douglas Squirrel'
  s.email = 'ds@douglassquirrel.com'
  s.rubyforge_project = 'None'
end

gem_file_name = Gem::Builder.new(gemspec).build

output_dir = ARGV[0]
FileUtils.move(gem_file_name, output_dir)
