# Moving GnuPG Keys 

From machine with secret keys:

`gpg --batch --export-secret-key "<KEY UUID>" | ssh <NEW MACHINE IP> gpg --import --batch`
