#!/usr/bin/ruby

require 'fileutils'

output_dir = ARGV[0]
FileUtils.copy('kropotkin-ruby', output_dir)
