#!/usr/bin/python
from glob import glob
from os import access, path, X_OK
from os.path import isdir, join
from subprocess import Popen

def start_component(directory):
    nodes = set(glob(join(directory, "*")))
    dirs = set([f for f in nodes if isdir(f)])
    files = nodes - dirs

    for d in dirs:
        start_component(join(directory, d))

    executables = [f for f in files if access(join(directory, f), X_OK)]
    if (len(executables) != 1):
        print "%d executables in %s" % (len(executables), directory)
    else:
        print "Executing: %s" % executables[0]
        Popen(executables[0], cwd=directory)

if __name__=="__main__":
    print "Running start_component component"
