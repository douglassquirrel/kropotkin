#!/usr/bin/python
from os import walk
from os.path import abspath, join
import sys

sys.path.append(abspath("core/factspace"))
sys.path.append(abspath("core/deployer"))
from factspace import start_factspace
from deployer import deploy

PORT=2001
KROPOTKIN_URL="http://localhost:%s" % PORT
start_factspace("Kropotkin", PORT, KROPOTKIN_URL)
for root, dirs, files in walk('core'):
    for d in dirs:
        deploy(join(root, d), KROPOTKIN_URL)
