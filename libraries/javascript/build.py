#!/usr/bin/python
from shutil import copy
from sys import argv

output_dir = argv[1]
copy('kropotkin-js', output_dir)
