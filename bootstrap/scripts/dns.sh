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
elif [[ $os == "arch" ]]; then
  sudo rm /etc/dnsmasq.conf
  sudo rm /etc/dnscrypt-proxy.toml
  sudo ln -s ~/dotfiles/conf/dns/dnsmasq.conf /etc/
  sudo ln -s ~/dotfiles/conf/dns/dnscrypt-proxy.toml /etc/
  sudo ln -s ~/dotfiles/conf/dns/dnsmasq.d/testing.conf /etc/dnsmasq.d/
  sudo systemctl enable dnsmasq
  sudo systemctl start dnsmasq
  sudo systemctl enable dnscrypt-proxy
  sudo systemctl start dnscrypt-proxy
fi

dig test.foo @127.0.0.1
dig google.com @127.0.0.1

echo "Assuming the two dig command above ran, you can set your DNS to 127.0.0.1"
