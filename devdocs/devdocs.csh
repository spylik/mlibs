#!/bin/tcsh
set projectdir = `dirname $0`
cd $projectdir
bundle exec rackup
