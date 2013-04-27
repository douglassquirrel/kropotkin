#!/usr/bin/python
from os import walk
from os.path import abspath, join
from tempfile import mkdtemp
import sys

from core.deployer.deployer import deploy
from core.factspace.factspace import start_factspace
from core.publisher.publisher import publish

PORT=2001
KROPOTKIN_URL="http://localhost:%s" % PORT
KROPOTKIN_DIR=mkdtemp()

start_factspace("Kropotkin", PORT, KROPOTKIN_URL)
env = {'KROPOTKIN_DIR': KROPOTKIN_DIR}
for root, dirs, files in walk('core'):
    for d in dirs:
        deploy(d, join(root, d), KROPOTKIN_URL, env)
