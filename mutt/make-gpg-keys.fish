#!/usr/bin/env fish

function encrypt_keys
  read -s -l -p 'set_color green; echo -n "Password for oncomouse@gmail.com"; set_color normal; echo "> "' gmail_password

  read -s -l -p 'set_color green; echo -n "Password for apilsch@tamu.edu"; set_color normal; echo "> "' tamu_password

  echo $gmail_password | gpg --use-agent -e -r "Andrew Pilsch" > ~/.passwords/oncomouse.gmail.com.gpg
  echo $tamu_password | gpg --use-agent -e -r "Andrew Pilsch" > ~/.passwords/apilsch.tamu.edu.gpg
end

function make_key
  set -l passphrase (shuf -n 15 /usr/share/dict/words | tr "\n" " ")
  printf "\
     \%echo Generating a basic OpenPGP key
     Key-Type: RSA
     Key-Length: 2048
     Subkey-Type: ELG-E
     Subkey-Length: 1024
     Name-Real: Andrew Pilsch
     Name-Comment: *shrug*
     Name-Email: andrew@pilsch.com

     Expire-Date: 0
     Passphrase: ($passphrase)
     # Do a commit here, so that we can later print 'done' :-)
     \%commit
     \%echo done" > key-dets
  gpg --batch --generate-key key-dets
  rm key-dets
  echo -n "Your key passphrases is ($passphrase). Write it down and press enter to continue."
  read -p "echo ''" foo
end

make_key()
encrypt_keys()
