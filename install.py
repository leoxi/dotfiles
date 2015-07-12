#!/usr/bin/env python
import os


def link_file(filename, target="~/", hidden=True):
    symbol = "." if hidden else ""
    link = os.path.expanduser(target) + symbol + filename
    if os.path.lexists(link):
        os.remove(link)
    os.symlink(os.path.realpath(filename), link)
    print "link %s done." % link


if __name__ == "__main__":
    link_file("gitconfig")
    link_file("gitignore_global")
    link_file("zshrc")
    link_file("emacs.d")
    link_file("plugins.sbt", target="~/.sbt/0.13/plugins/", hidden=False)
