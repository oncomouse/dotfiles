[Here's a fix](https://d.sb/2016/11/gpg-inappropriate-ioctl-for-device-errors)

To solve the problem, you need to enable loopback pinentry mode. Add this to ~/.gnupg/gpg.conf:

```
use-agent
pinentry-mode loopback
```

And add this to ~/.gnupg/gpg-agent.conf, creating the file if it doesn't already exist:

```
allow-loopback-pinentry
```
