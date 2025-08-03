#!/bin/bash
cd /home/schalk/git/compiler
echo "Test.all" | timeout 600 stack ghci 2>&1