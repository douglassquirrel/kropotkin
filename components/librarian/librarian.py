#!/usr/bin/python
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

while True:
    fact = get_oldest_fact_and_stamp('kropotkin', 'library_available',
                                     {}, 'librarian.223')
    if not fact:
        continue

    original_dir = fact['directory']
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
    file_location = join(output_dir, files[0])

    if not store_fact('kropotkin', 'component_available',
                      {'location': file_location}):
        print 'Could not store component_available for %s' % output_dir
