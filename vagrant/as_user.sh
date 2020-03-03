#!/bin/sh

echo "export PATH=\"$HOME/.local/bin:$PATH\"" >>~vagrant/.bashrc
mkdir /vagrant/data
ln -s /vagrant/data ~vagrant/.mtlstats

cd /vagrant
stack install
