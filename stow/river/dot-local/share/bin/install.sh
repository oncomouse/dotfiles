#!/bin/sh

echo -e "This will remove your previous configuration assuming it is a symlink, are you sure? y - confirm; any letter - to abort"

read answer

if [[ $answer == "y" ]] || [[ $answer == "Y" ]]; then

    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

    for i in {river,mako,eww,foot,wofi}; do
        CONF_DIR="$HOME/.config/$i"
        if [ -d  $CONF_DIR ]; then
            echo "Found configuration for $i"
            BACKUP_DIR=$HOME/.config/riverconfigbackup
            echo "Putting it in $BACKUP_DIR"
            mkdir -p "$BACKUP_DIR"
            cp $CONF_DIR "$BACKUP_DIR" -frv
            rm -rfv $CONF_DIR
            cp $SCRIPT_DIR/$i $CONF_DIR -frv
        else
            # Check if it is a symlink file
            echo "Checking if there is an exiting symlink..."
            if [ -L $CONF_DIR ]; then
                echo "Removing symlink"
                rm $CONF_DIR -v
                cp $SCRIPT_DIR/$i $HOME/.config -rfv
            else
                echo "No symlinks, copying configs..."
                cp $SCRIPT_DIR/$i $HOME/.config -rfv
            fi
        fi
    done
    cp $SCRIPT_DIR/mygrimshot.sh $HOME/.local/bin -fv
    cp $SCRIPT_DIR/wayriver.sh $HOME/wayriver.sh -fv
    echo "Done copying configs"
    echo "DONE"
else
    echo "Aborted"
    exit
fi


