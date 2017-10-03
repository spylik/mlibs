# To use with tcsh:
#
# 1. put gitprompt.sh to ~/bin/gitprompt.csh
# 2. add to the ~/.tcshrc or ~/.cshrc:
# alias precmd "source ~/bin/gitprompt.csh"

setenv GIT_BRANCH_CMD "sh -c 'hg branch 2> /dev/null || git rev-parse --abbrev-ref HEAD 2> /dev/null' | sed -e 's/^/:/'"
set prompt  = "%B[%T %{\033[32m%}%n%{\033[0m%}@%{\033[31m%}%m%{\033[33m%}%/%{\033[1;36m%}`$GIT_BRANCH_CMD`%{\033[0m%}] "
