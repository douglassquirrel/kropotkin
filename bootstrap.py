#!/usr/bin/python
from os import walk
from os.path import abspath, join
import sys

sys.path.append(abspath("core/deployer"))
sys.path.append(abspath("core/factspace"))
sys.path.append(abspath("core/publisher"))
from deployer import deploy
from factspace import start_factspace
from publisher import publish

PORT=2001
KROPOTKIN_URL="http://localhost:%s" % PORT
start_factspace("Kropotkin", PORT, KROPOTKIN_URL)
for root, dirs, files in walk('core'):
    for d in dirs:
        deploy(d, join(root, d), KROPOTKIN_URL)
