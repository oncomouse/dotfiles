# Moving GnuPG Keys 

From machine with secret keys:

`gpg --export-secret-key "<KEY UUID>" | ssh <NEW MACHINE IP> set -x GPG_TTY (tty) && gpg --import`
