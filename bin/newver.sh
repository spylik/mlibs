#!/bin/sh

while getopts ":Mmp" Option
do
  case $Option in
    M ) major=true;;
    m ) minor=true;;
    p ) patch=true;;
  esac
done

for file in $(find src -type f -name \*.app.src); do
	old="$(cat $file | grep -o '{vsn\s*,\s*"\d*.\d*.\d*"}' | grep -o '"\d*.\d*.\d*"' | sed -e 's/^"//' -e 's/"$//')"
	ar=( ${old//./ } )
	if [ ! -z $major ]; then ((ar[0]++));ar[1]=0;ar[2]=0;fi
	if [ ! -z $minor ]; then ((ar[1]++));ar[2]=0;fi
	if [ ! -z $patch ]; then ((ar[2]++));fi
	new="${ar[0]}.${ar[1]}.${ar[2]}"
	echo "Patch file $file. Old vsn: $old, new vsn: $new"
	sed -i.bak s/$old/$new/g $file
	rm $file.bak
	git add $file
	if [ -e "relx.config" ]; then echo "Patch file relx.config. New vsn: $new"; sed -i.bak s/$old/$new/g relx.config;rm relx.config.bak;git add relx.config;fi
done;
