# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|
  config.vm.box = "ubuntu/xenial64"
  config.vm.provision :shell, path: "vagrant/provision.sh"
  config.vm.provider :virtualbox do |v|
    v.customize ["modifyvm", :id, "--memory", 2048]
  end
end
