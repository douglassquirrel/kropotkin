# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)

require 'kropotkin/version'

Gem::Specification.new do |spec|
  spec.name          = "kropotkin"
  spec.version       = Kropotkin::VERSION
  spec.authors       = ["Douglas Squirrel"]
  spec.email         = ["ds@douglassquirrel.com"]
  spec.summary       = 'Interface to Kropotkin framework.'
  spec.description = 'Provides methods for creating Kropotkin factspaces, ' \
                + 'storing facts in them, and querying facts from them.'
  spec.homepage = 'https://github.com/douglassquirrel/kropotkin'
  spec.license       = "MIT"
  spec.rubyforge_project = "nowarning"

  spec.files = ["lib/kropotkin.rb", "lib/kropotkin/version.rb"]
  spec.require_paths = ["lib"]

  spec.add_development_dependency "bundler", "~> 1.3"
  spec.add_development_dependency "rake"
end
