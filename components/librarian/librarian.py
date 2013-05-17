#!/usr/bin/python
from base64 import b64encode
from kropotkin import get_oldest_fact_and_stamp, store_fact
from os import access, listdir, X_OK
from os.path import basename, isdir, join
from shutil import copy, copytree
from subprocess import Popen
from tempfile import mkdtemp

def copy_all(original_dir, dest_dir):
    for f in listdir(original_dir):
        original = join(original_dir, f)
        dest = join(dest_dir, f)
        if isdir(original):
            copytree(original, dest)
        else:
            copy(original, dest)

def get_unique_executable(directory):
    nodes = listdir(directory)
    executables = [f for f in nodes if is_executable_file(join(directory, f))]
    return executables[0] if len(executables) == 1 else None

def is_executable_file(f):
    return (not isdir(f)) and access(f, X_OK)

MODULE_TYPES = {'python':     'python-module',
                'javascript': 'javascript-library',
                'ruby':       'ruby-gem'}

while True:
    fact = get_oldest_fact_and_stamp('kropotkin', 'library_available',
                                     {}, 'librarian')
    if not fact:
        continue

    original_dir = fact['directory']
    language = fact['language']

    build_dir = mkdtemp(prefix='library-build-%s' % basename(original_dir))
    output_dir = mkdtemp(prefix='library-%s' % basename(original_dir))
    copy_all(original_dir, build_dir)

    executable = get_unique_executable(build_dir)
    if not executable:
        print "Cannot locate unique executable in %s" % build_dir
        continue

    process = Popen([executable, output_dir], cwd=build_dir)
    process.wait()
    files = listdir(output_dir)
    if len(files) != 1:
        print 'No single file output for library %s' % basename(original_dir)
        continue
    output_file = join(output_dir, files[0])

    with open(output_file) as f:
        bytes = b64encode(f.read())
    content = {'name': basename(output_file),
               'language': language,
               'content_type': MODULE_TYPES[language],
               'bytes': bytes}
    if not store_fact('kropotkin', 'component', content):
        print 'Could not store component for %s' % name
