# Installation of MicSE

## Table of Contents

- [Installation of MicSE](#installation-of-micse)
  - [Table of Contents](#table-of-contents)
  - [Vagrant Box](#vagrant-box)
    - [Prerequisite](#prerequisite)
    - [Customize Virtual Machine](#customize-virtual-machine)
    - [Manage Virtual Machine](#manage-virtual-machine)
  - [From Source](#from-source)
    - [Setup dependencies](#setup-dependencies)
    - [Clone, Build](#clone-build)

## Vagrant Box

MicSE provides a Vagrant Box.
This box is based on Ubuntu 20.04 LTS (Focal Fossa) v20210304.0.0.
And, provider of this box is only VirtualBox now.

### Prerequisite

- [Vagrant](https://www.vagrantup.com/docs/installation)
- [VirtualBox](https://www.virtualbox.org/wiki/Downloads)

### Customize Virtual Machine

If you want to customize virtual machine (e.g., size of disk, memory, number of cores, ...) depending on your system spec,
you can modify following parts of [`Vagrantfile`](../Vagrantfile) for such purpose.

```ruby
...
# Default Disk Size: 40GB
config.disksize.size = "40GB"

# Provider settings: VirtualBox
config.vm.provider "virtualbox" do |vb|
  ...
  # Default Memory Size: 4GB
  vb.memory = 4096
  # Default Cores: 4
  vb.cpus = 4
end
...
```

### Manage Virtual Machine

```bash
# Create or load virtual machine with Vagrant box
$ vagrant up
Bringing machine 'micse' up with 'virtualbox' provider...
...

# Connect to the machine
$ vagrant ssh
Welcome to Ubuntu 20.04.2 LTS (GNU/Linux 5.4.0-66-generic x86_64)
... # Project directory is mounted to `~/MicSE` directory

# Halt the machine after exit the connection
$ vagrant halt
==> micse: Attempting graceful shutdown of VM...
...

# Destroy and delete the machine
$ vagrant destroy
    micse: Are you sure you want to destroy the 'micse' VM? [y/N] y
==> micse: Destroying VM and associated drives...
...
```

If you don't need bootstrapping when you run the machine, load machine with option `--no-provision`.

```bash
# Create or load virtual machine without bootstrapping
$ vagrant up --no-provision
```

## From Source

### Setup dependencies

MicSE uses these packages.

| System Package Name | Version |
| :------------------ | :-----: |
| make                | ^4.2.1  |
| ocaml               | =4.10.0 |
| opam                | ^2.0.5  |

| Opam Package Name |  Version  |
| :---------------- | :-------: |
| Batteries         |  =3.3.0   |
| Core              |  =0.14.1  |
| Dune              |  =2.4.0   |
| Menhir            | =20210419 |
| Ocamlgraph        |  =2.0.0   |
| Ptime             |  =0.8.5   |
| Yojson            |  =1.7.0   |
| Z3                |  =4.8.13  |
| Zarith            |   =1.12   |
| OUnit2            |  =2.2.4   |
| BigNum            | =v0.14.0  |
| ppx_deriving      |  =5.2.1   |
| Mtime             |  =1.2.0   |
| Logs              |  =0.7.0   |

### Clone, Build

We are not providing the version build file now.
To use the tool of MicSE, you have to clone this repository and build it manually.

```bash
$ git clone https://github.com/kupl/MicSE.git
$ cd MicSE
$ make
dune build
...
```
