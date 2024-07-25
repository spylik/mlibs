if [ ! -z $SSH_CONFIG_FILE ] ; then
    echo "good"
    exec ssh -F $SSH_CONFIG_FILE $argv
else
    exec ssh $argv
fi
