# Configure SSH on Systems

1. `ssh-keygen -t ed25519` to generate key
1. Edit `~/.ssh/config` and add an entry for the host:
```
Host <IP ADDR>
	IgnoreUnknown AddKeysToAgent,UseKeychain
	AddKeysToAgent yes
	UseKeychain yes
	IdentityFile ~/.ssh/id_ed25519
```
1. Run `ssh-add ~/.ssh/id_ed25519` and enter passphrase
1. Run `ssh-copy-id <IP ADDR>`
