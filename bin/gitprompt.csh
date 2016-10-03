setenv GIT_BRANCH_CMD "sh -c 'git branch --no-color 2> /dev/null' | sed -e '/^[^*]/d' -e 's/* \(.*\)/:\1/'"
set prompt  = "%B[%T %{\033[32m%}%n%{\033[0m%}@%{\033[31m%}%m%{\033[33m%}%/%{\033[1;36m%}`$GIT_BRANCH_CMD`%{\033[0m%}] "
