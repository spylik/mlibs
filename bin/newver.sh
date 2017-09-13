#!/bin/sh

# sh-driven cross-vcs hook for automatically update version for Erlang and Elixir projects.
#
# It uses the following strategy:
# - patch part of version updates during each commit.
# - minor part of version updates during each merge.
# - major part updates manually only. for update run path_to_newver/newver.sh -M
#
# Currently support git and mercurial as source version control system

while getopts ":Mmp" Option
do
  case $Option in
    M ) major=true;;
    m ) minor=true;;
    p ) patch=true;;
  esac
done

nv() {
	ar=( ${old//./ } )
	if [ ! -z $major ]; then ((ar[0]++));ar[1]=0;ar[2]=0;fi
	if [ ! -z $minor ]; then ((ar[1]++));ar[2]=0;fi
	if [ ! -z $patch ]; then ((ar[2]++));fi
	echo "${ar[0]}.${ar[1]}.${ar[2]}"
}

# Determine which version control system we using for this project
add_to_repo(){
	if [ -d ".git" ]; then
		git add $file
	fi
	if [ -d ".hg" ]; then
		hg commit --amend $file
	fi
}

if [ -e "mix.exs" ]; then
	file="mix.exs"
	echo "Found mix.exs (Elixir project)"
	old=$(cat $file | awk '/version:/{print $2;}' | tr -d '",')
	new=$(nv $old)
	echo "Patching file $file. Old vsn: $old, new vsn: $new"
	sed -i.bak s/$old/$new/g $file
	rm $file.bak
	add_to_repo $file
else
	if [ -d "src" ]; then
		echo "Found src folder.. (probably Erlang project)"
		for file in $(find src -type f -name \*.app.src); do
			old="$(cat $file | grep -o '{vsn\s*,\s*"\d*.\d*.\d*"}' | grep -o '"\d*.\d*.\d*"' | sed -e 's/^"//' -e 's/"$//')"
			new=$(nv $old)
			echo "Patching file $file. Old vsn: $old, new vsn: $new"
			sed -i.bak s/$old/$new/g $file
			rm $file.bak
			add_to_repo $file
			if [ -e "relx.config" ]; then echo "Patch file relx.config. New vsn: $new"; sed -i.bak s/$old/$new/g relx.config;rm relx.config.bak;add_to_repo relx.config;fi
		done;
	fi
fi
