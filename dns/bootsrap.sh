#!/usr/bin/env bash

brew bundle install --no-lock

mkdir -p /usr/local/etc/dnsmasq.d
rm /usr/local/etc/dnsmasq.conf
rm /usr/local/etc/dnscrypt-proxy.toml
ln -s ~/dotfiles/dns/dnsmasq.conf /usr/local/etc/
ln -s ~/dotfiles/dns/dnscrypt-proxy.toml /usr/local/etc/
ln -s ~/dotfiles/dns/dnsmasq.d/testing.conf /usr/local/etc/dnsmasq.d/

sudo brew services start dnsmasq
sudo brew services start dnscrypt-proxy

dig test.foo @127.0.0.1
dig google.com @127.0.0.1

echo "Assuming the two dig command above ran, you can set your DNS to 127.0.0.1"
