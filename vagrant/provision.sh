#!/bin/sh

apt-get update
apt-get upgrade
apt-get -y install libghc-ncurses-dev
wget -qO- https://get.haskellstack.org/ | sh

export HOME=/home/vagrant

sudo -u vagrant /vagrant/vagrant/as_user.sh
