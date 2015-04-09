#!/usr/bin/env python
import os

ignores = [".git", ".DS_Store", "install.py"]
files = os.listdir(".")
for i in files:
    if i not in ignores:
        link = os.path.expanduser("~/") + "." + i
        if os.path.lexists(link):
            os.remove(link)
        os.symlink(os.path.realpath(i), link)
        print "link %s done." % link
