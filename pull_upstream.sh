#!/bin/bash
git remote add upstream git://github.com/rsetienne/PBD.git
git fetch origin -v; git fetch upstream -v; git merge upstream/master