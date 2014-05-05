#!/bin/bash

git submodule sync
git submodule init
git submodule update
git submodule foreach git pull origin master
echo "Initing submodules"
git submodule foreach git submodule init
echo "Updating submodules"
git submodule foreach git submodule update
