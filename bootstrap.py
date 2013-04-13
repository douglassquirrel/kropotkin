#!/usr/bin/python
from os.path import abspath
import sys

sys.path.append(abspath("core/factspace"))
sys.path.append(abspath("core/start_component"))
import factspace
import start_component

factspace.start_factspace("Kropotkin", 2001, "http://localhost:2001")
start_component.start_component(abspath("core"))
