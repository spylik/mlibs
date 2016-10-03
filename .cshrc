# $FreeBSD: src/share/skel/dot.cshrc,v 1.10.2.3 2001/08/01 17:15:46 obrien Exp $
#
# .cshrc - csh resource script, read at beginning of execution by each shell
#
# see also csh(1), environ(7).
#

alias h		history 25
alias j		jobs -l
alias la	ls -a
alias lf	ls -FA
alias ll	ls -lA
alias bc bc -l
alias ls	ls -G
alias portupdate /opt/local/bin/port -v selfupdate
alias radichproxy 'ssh -D 2001 -l xlet 31.131.16.244'
alias pta 'escript ~spyl/projects/parse_trans/ebin/parse_trans_pp.beam'

alias precmd "source ~/bin/gitprompt.csh"

setenv GIT_BRANCH_CMD "sh -c 'git branch --no-color 2> /dev/null' | sed -e '/^[^*]/d' -e 's/* \(.*\)/:\1/'"
# A righteous umask
umask 22

set path = (/sbin /bin /usr/sbin /usr/bin /usr/games /usr/local/sbin /usr/local/bin /usr/X11R6/bin /opt/sbin /opt/bin /opt/local/sbin /opt/local/bin $HOME/bin /opt/local/lib/percona/bin)

setenv VLC_PLUGIN_PATH /opt/local/lib/vlc/plugins/

#setenv 	JAVA_HOME	/usr/

set color
setenv	EDITOR	vim
#set printexitvalue 
#set watch = (any any)
#w
setenv	PAGER	less
setenv	BLOCKSIZE	K
setenv	CLICOLOR	YES	

setenv LC_ALL en_US.UTF-8  
setenv LANG en_US.UTF-8
#setenv AWS_CONFIG_FILE ~spyl/.awsconfig

set MainC  = '^[[=2F^[[=0G'
set blackC = '^[[0m^[[30m'
set redC   = '^[[0m^[[31m'
set greenC = '^[[0m^[[32m'
set yelloC = '^[[0m^[[33m'
set blueC  = '^[[0m^[[34m'
set magenC = '^[[0m^[[35m'
set cyanC  = '^[[0m^[[36m'
set whiteC = '^[[0m^[[37m'
									

if ($?prompt) then
	# An interactive shell -- set some stuff up
	set prompt = "%B[%T %{\033[32m%}%n%{\033[0m%}@%{\033[31m%}%m%{\033[33m%}%/%{\033[0m%}] "
	set filec
	set history = 100
	set savehist = 1000
	set mail = (/var/mail/$USER)
	if ( $?tcsh ) then
		bindkey "^W" backward-delete-word
		bindkey -k up history-search-backward
		bindkey -k down history-search-forward
	endif
endif

setenv PATH /opt/local/bin:/opt/local/sbin:$PATH
