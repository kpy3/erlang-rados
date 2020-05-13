Ceph RADOS client for Erlang
============================

A simple client to [Ceph](https://ceph.io) RADOS.
It has simplified interface to work with selected
pools in cluster, leaving configuration and administration
elsewhere.

Requirements
------------

Since Ceph is not a kind of software which can bw deployed
on development machine, vagrant and VirtualBox are required.
This project's box is being tested and known to work on vagrant
version 2.2.6+ with VirtualBox 6.0.1+.

Vagrand box build on top of Ubuntu 18.04 LTS with following software:
- Ceph 14.2.9 codenamed Nautilus
- Erlang 22.3.1
- rebar3 3.13.1

Build
-----

If you don't have Ceph or librados installed in you development
environment use vagrant box. To setup and start box run

    $ vagrant up

from project root, then login to box with

    $ vagrant ssh

and change directory to /vagrant, thens run

    $ rebar3 compile
