# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import glob, messageboard, os.path, sys, subprocess, time

def basename_without_extension(file_path):
    return os.path.splitext(os.path.basename(file_path))[0]

def start_directly(file_path):
    print "Starting process directly using file %s" % file_path
    with open(file_path, "r") as f:
        code = f.read()
    p = subprocess.Popen(args="python", stdin=subprocess.PIPE)
    p.stdin.write(code)
    p.stdin.close()
    print "Process using file %s now running" % file_path

def start(file_path):
    verb = basename_without_extension(file_path)
    print "Starting %s, using verb %s, via start_process message" % (file_path, verb)
    with open(file_path, "r") as f:
        code = f.read()
    messageboard.post("start_process", str({'verb':verb, 'code':code}))

app_directory_path = sys.argv[1] if len(sys.argv) >= 2 else os.path.expanduser("~/kropotkin-app")
print "Starting bootstrap using app directory %s" % app_directory_path

files_to_start_directly = ["start_process.py"]
for file_path in files_to_start_directly:
    start_directly(file_path)
time.sleep(1)

files_to_start_normally = glob.glob(app_directory_path + "/*.py")
for file_path in files_to_start_normally:
    start(file_path)

print "Bootstrap finished"
while 1:
    pass
