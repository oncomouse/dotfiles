#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
if [[ $os == "macos" ]];then
  mkdir -p /usr/local/etc/dnsmasq.d
  rm /usr/local/etc/dnsmasq.conf
  rm /usr/local/etc/dnscrypt-proxy.toml
  ln -s ~/dotfiles/conf/dns/dnsmasq.conf /usr/local/etc/
  ln -s ~/dotfiles/conf/dns/dnscrypt-proxy.toml /usr/local/etc/
  ln -s ~/dotfiles/conf/dns/dnsmasq.d/testing.conf /usr/local/etc/dnsmasq.d/

  sudo brew services start dnsmasq
  sudo brew services start dnscrypt-proxy

  dig test.foo @127.0.0.1
  dig google.com @127.0.0.1

  echo "Assuming the two dig command above ran, you can set your DNS to 127.0.0.1"
fi
