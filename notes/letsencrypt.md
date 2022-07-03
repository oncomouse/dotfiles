# Setting Up Lets Encrypt with port 80 blocked

Get `dehydrated`:

```
curl -so dehydrated https://raw.githubusercontent.com/lukas2511/dehydrated/master/dehydrated
```

Do all of this as root.

Install it to `/usr/bin` (`chmod +x dehydrated; cp dehydrated /usr/bin; chown root:root /usr/bin/dehydrated`).

Create `/usr/bin/update-certs`:

```
#!/usr/bin/env sh
systemctl stop nginx
python /etc/dehydrated/alpn-server.py &
dehydrated -c -f /etc/dehydrated/config
pkill -f alpn
systemctl start nginx
#vim: ft=sh
```

Run `chmod +x /usr/bin/update-certs`

Create the dehydrated configuration directory: `mkdir -p /etc/dehydrated`.

In that directory, run `openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 10000 -nodes` to generate the dummy certificate for the challenge.

Create `/etc/dehydrated/alpn-server.py` with the following contents:

```python
import ssl
import socketserver
import threading
import re
import os

ALPNDIR="/etc/dehydrated/alpn-certs"
PROXY_PROTOCOL=False

FALLBACK_CERTIFICATE="/etc/dehydrated/cert.pem"
FALLBACK_KEY="/etc/dehydrated/key.pem"

class ThreadedTCPServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
    pass

class ThreadedTCPRequestHandler(socketserver.BaseRequestHandler):
    def create_context(self, certfile, keyfile, first=False):
        ssl_context = ssl.create_default_context(ssl.Purpose.CLIENT_AUTH)
        ssl_context.set_ciphers('ECDHE+AESGCM')
        ssl_context.set_alpn_protocols(["acme-tls/1"])
        ssl_context.options |= ssl.OP_NO_TLSv1 | ssl.OP_NO_TLSv1_1
        if first:
            ssl_context.set_servername_callback(self.load_certificate)
        ssl_context.load_cert_chain(certfile=certfile, keyfile=keyfile)
        return ssl_context

    def load_certificate(self, sslsocket, sni_name, sslcontext):
        print("Got request for %s" % sni_name)
        if not re.match(r'^(([a-zA-Z]{1})|([a-zA-Z]{1}[a-zA-Z]{1})|([a-zA-Z]{1}[0-9]{1})|([0-9]{1}[a-zA-Z]{1})|([a-zA-Z0-9][-_.a-zA-Z0-9]{0,61}[a-zA-Z0-9]))\.([a-zA-Z]{2,13}|[a-zA-Z0-9-]{2,30}.[a-zA-Z]{2,3})$', sni_name):
            return

        certfile = os.path.join(ALPNDIR, "%s.crt.pem" % sni_name)
        keyfile = os.path.join(ALPNDIR, "%s.key.pem" % sni_name)

        if not os.path.exists(certfile) or not os.path.exists(keyfile):
            return

        sslsocket.context = self.create_context(certfile, keyfile)

    def handle(self):
        if PROXY_PROTOCOL:
            buf = b""
            while b"\r\n" not in buf:
                buf += self.request.recv(1)

        ssl_context = self.create_context(FALLBACK_CERTIFICATE, FALLBACK_KEY, True)
        newsock = ssl_context.wrap_socket(self.request, server_side=True)

if __name__ == "__main__":
    HOST, PORT = "0.0.0.0", 443

    server = ThreadedTCPServer((HOST, PORT), ThreadedTCPRequestHandler, bind_and_activate=False)
    server.allow_reuse_address = True
    try:
        server.server_bind()
        server.server_activate()
        server.serve_forever()
    except:
        server.shutdown()
```

Create `/etc/dehydrated/config`, with the following contents:

```
CHALLENGETYPE="tls-alpn-01"
BASEDIR="/etc/dehydrated"
CERTDIR="/etc/nginx/certs"
CONTACT_EMAIL=<YOUR EMAIL>
```

Create `/etc/dehydrated/domains.txt` with a list of domain names you want to request certificates for, one-per-line.

Run `mkdir -p /var/www/dehydrated`

Register for the account by running: `./dehydrated --register --accept-terms`

Then run `/usr/bin/update-certs`.

You will need to configure Nginx to use your new certificate files.

## Systemd Timer

Create `/etc/systemd/system/update-certs.service`

```
[Unit]
Description=Update LetsEncrypt Certificates

[Service]
Type=oneshot
ExecStart=/usr/bin/update-certs
```

Create `/etc/systemd/system/update-certs.timer`

```
[Unit]
Description=Run update-certs monthly

[Timer]
OnCalendar=*-*-01 04:00:00

[Install]
WantedBy=timers.target
```

Run `systemctl start update-certs.timer`
