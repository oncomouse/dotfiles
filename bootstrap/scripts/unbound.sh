#!/usr/bin/env bash
os=$(bash ~/dotfiles/bootstrap/scripts/os.sh)
if [[ $os == "macos" ]];then
  root="/usr/local"
  user_id=$(dscl . -list /Users UniqueID | grep _unbound | sed -e "s/.* //g")
  group_id=$(dscl . -list /Groups PrimaryGroupID | grep _unbound | sed -e "s/.* //g")
  if [[ $user_id == '' ]];then
    users=$(dscl . -list /Users UniqueID)
    groups=$(dscl . -list /Groups PrimaryGroupID)
    user_id=-1
    group_id=-1
    for i in $(seq 100 500);do
      if [[ $(echo "$users" | grep -c "$i") == 0 ]]; then
        user_id=$i
      fi
      if [[ $(echo "$groups" | grep -c "$i") == 0 ]]; then
        group_id=$i
      fi
      if [[ "$group_id" != -1 && "$user_id" != -1 ]]; then
        break
      fi
    done
    if [[ "$user_id" == -1 || "$group_id" == -1 ]]; then
      echo "No User IDs or Group IDs available"
      exit
    fi
    sudo dscl . -create /Groups/_unbound
    sudo dscl . -create /Groups/_unbound PrimaryGroupID "$group_id"
    sudo dscl . -create /Users/_unbound
    sudo dscl . -create /Users/_unbound RecordName _unbound unbound
    sudo dscl . -create /Users/_unbound RealName "Unbound DNS server"
    sudo dscl . -create /Users/_unbound UniqueID "$user_id"
    sudo dscl . -create /Users/_unbound PrimaryGroupID "$group_id"
    sudo dscl . -create /Users/_unbound UserShell /usr/bin/false
    sudo dscl . -create /Users/_unbound Password '*'
    sudo dscl . -create /Groups/_unbound GroupMembership _unbound
  fi
elif [[ $os == "arch" ]]; then
  root=""
  sudo useradd _unbound
fi
sudo unbound-anchor -4 -a "${root}/etc/unbound/root.key"
sudo unbound-control-setup -d "${root}/etc/unbound"
sudo curl --output "${root}/etc/unbound/root.hints" https://www.internic.net/domain/named.cache
(curl --silent https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling-porn-social/hosts | grep '^0\.0\.0\.0' | sort) | awk '{print "local-zone: \""$2"\" refuse"}' | sudo tee "${root}/etc/unbound/zone-block-general.conf"
cat << EOF | sudo tee "${root}/etc/unbound/unbound.conf"
server:
	# log verbosity
	verbosity: 1
	interface: 127.0.0.1
	access-control: 127.0.0.1/8 allow
	chroot: ""
	username: "_unbound"
	auto-trust-anchor-file: root.key
	root-hints: root.hints
	# answer DNS queries on this port
	port: 53
	# enable IPV4
	do-ip4: yes
	# disable IPV6
	do-ip6: no
	# enable UDP
	do-udp: yes
	# enable TCP, you could disable this if not needed, UDP is quicker
	do-tcp: yes
	# which client IPs are allowed to make (recursive) queries to this server
	access-control: 10.0.0.0/8 allow
	access-control: 127.0.0.0/8 allow
	access-control: 192.168.0.0/16 allow
	# do not answer id.server and hostname.bind queries
	hide-identity: yes
	# do not answer version.server and version.bind queries
	hide-version: yes
	# will trust glue only if it is within the servers authority
	harden-glue: yes
	# require DNSSEC data for trust-anchored zones, if such data
	# is absent, the zone becomes  bogus
	harden-dnssec-stripped: yes
	# use 0x20-encoded random bits in the query to foil spoof attempts
	use-caps-for-id: yes
	# the time to live (TTL) value lower bound, in seconds
	cache-min-ttl: 3600
	# the time to live (TTL) value cap for RRsets and messages in the cache
	cache-max-ttl: 86400
	# perform prefetching of close to expired message cache entries
	prefetch: yes
	num-threads: 4
	msg-cache-slabs: 8
	rrset-cache-slabs: 8
	infra-cache-slabs: 8
	key-cache-slabs: 8
	rrset-cache-size: 256m
	msg-cache-size: 128m
	so-rcvbuf: 1m
	private-address: 192.168.0.0/16
	private-address: 172.16.0.0/12
	private-address: 10.0.0.0/8
	private-domain: "home.lan"
	unwanted-reply-threshold: 10000
	val-clean-additional: yes
	# additional blocklist (Steven Black hosts file, read above)
	include: ${root}/etc/unbound/zone-block-general.conf
remote-control:
	control-enable: yes
	control-interface: 127.0.0.1
	server-key-file: "${root}/etc/unbound/unbound_server.key"
	server-cert-file: "${root}/etc/unbound/unbound_server.pem"
	control-key-file: "${root}/etc/unbound/unbound_control.key"
	control-cert-file: "${root}/etc/unbound/unbound_control.pem"
forward-zone:
	name:"."
	# use Quad9
	forward-addr:9.9.9.9@853
	# or Cloudflare
	# forward-addr:1.1.1.1@853
	forward-ssl-upstream:yes
EOF
sudo chown -R _unbound:staff "${root}/etc/unbound"
sudo chmod 640 "${root}"/etc/unbound/*
if [[ $os == "macos" ]]; then
  sudo brew services start unbound
  networksetup -setdnsservers Wi-Fi 127.0.0.1
elif [[ $os == "arch" ]]; then
  sudo systemctl stop systemd-resolved
  sudo systemctl disable systemd-resolved  sudo systemctl enable unbound-resolvconf
  sudo systemctl enable unbound
  sudo systemctl start unbound-resolvconf
  sudo systemctl start unbound
  sudo rm /etc/resolv.conf
  cat << EOF | sudo tee /etc/resolv.conf
nameserver ::1
nameserver 127.0.0.1
options trust-adfi
EOF
fi
