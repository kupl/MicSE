# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  # Plugins: vagrant-disksize
  config.vagrant.plugins = ["vagrant-disksize"]

  # OS version: Ubuntu 20.04 LTS (Focal Fossa) v20210304.0.0
  config.vm.box = "ubuntu/focal64"
  config.vm.box_version = "20210304.0.0"

  # Default Disksize: 40GB
  config.disksize.size = "40GB"

  # Provider settings: VirtualBox
  config.vm.provider "virtualbox" do |vb|
    vb.name = "micse"
    vb.memory = 4096
    vb.cpus = 4
  end

  # Etc
  config.vm.hostname = "kupl"
  config.vm.define "micse"

  # Provisioning
  config.vm.provision "bootstrap", type: "shell",
      privileged: false, run: "always" do |bs|
    bs.path = "bootstrap.sh"
  end
end
