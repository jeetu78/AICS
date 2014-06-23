## Quick Start
-----------

To be up and running you need:

- [Vagrant >= 1.5.3](https://www.vagrantup.com/downloads.html)
- [VirtualBox >= 4.3.6](https://www.virtualbox.org/wiki/Downloads)
- [Git >= 1.8.5.2](http://git-scm.com/downloads)
- [Ansible >= 1.5.4](http://docs.ansible.com/intro_installation.html)

Install Vagrant plugins:

```Shell
> vagrant plugin install vagrant-aws
> vagrant plugin install vagrant-hostmanager
> vagrant plugin install vagrant-auto_network
```

Install Ansible Roles

```Shell
>ansible-galaxy install Mayeu.RabbitMQ
```

And make sure the MemSQL debian package is in `provision/memsql/files/`

Bootstrap the env:

```Shell
> vagrant up
```
