if [ ! -z $SSH_CONFIG_FILE ] ; then
    echo "$SSH_CONFIG_FILE in use"
    exec ssh -F "$SSH_CONFIG_FILE" "$@"
    exec ssh -F $SSH_CONFIG_FILE $argv
else
    exec ssh "$@"
fi
