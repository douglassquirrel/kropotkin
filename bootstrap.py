#!/usr/bin/python
from os.path import abspath
import sys
sys.path.append(abspath("core/factspace"))
sys.path.append(abspath("core/start_component"))
import factspace
import start_component

PORT=2001
KROPOTKIN_URL="http://localhost:%s" % PORT
factspace.start_factspace("Kropotkin", PORT, KROPOTKIN_URL)
start_component.start_component(abspath("core"), KROPOTKIN_URL)
