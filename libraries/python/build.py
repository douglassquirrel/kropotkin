#!/usr/bin/python
from distutils.core import setup
from errno import EEXIST
from os import chmod, makedirs, stat
from os.path import join
from stat import S_IXUSR
from sys import argv, exit

def copy_with_version(template, destination, version):
    with open(template, 'r') as t:
        template_text = t.read()
    with open(destination, 'w') as d:
        d.write(template_text.format(version=version))

def make_executable(filename):
    permissions = stat(filename).st_mode
    chmod(filename, permissions | S_IXUSR)

def ensure_exists(directory):
    try:
        makedirs(directory)
    except OSError as exception:
        if exception.errno != EEXIST:
            raise

with open('version.txt', 'r') as v:
    version = v.read().rstrip()

copy_with_version('setup.py.template', 'setup.py', version)

if len(argv) < 2:
    output_dir = 'dist'
else:
    output_dir = argv[1]
ensure_exists(output_dir)

print "Building Kropotkin python library"
print "Output directory: %s" % output_dir
print "Version: %s" % version

setup(name='kropotkin',
      version=version,
      py_modules=['kropotkin'],
      script_args=['-q', 'sdist', '--dist-dir', output_dir, '--formats', 'tar'],
      script_name='setup.py',
      url='https://github.com/douglassquirrel/kropotkin',
      author='Douglas Squirrel',
      author_email='ds@douglassquirrel.com')

if output_dir == 'dist':
    copied_install_file = join(output_dir, 'install.sh')
    copy_with_version('install.sh.template', copied_install_file, version)
    make_executable(copied_install_file)
