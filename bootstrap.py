#!/usr/bin/python
from os.path import abspath
import sys
sys.path.append(abspath("core/factspace"))
sys.path.append(abspath("core/deployer"))
from factspace import start_factspace
from deployer import deploy

PORT=2001
KROPOTKIN_URL="http://localhost:%s" % PORT
start_factspace("Kropotkin", PORT, KROPOTKIN_URL)
deploy(abspath("core"), KROPOTKIN_URL)
