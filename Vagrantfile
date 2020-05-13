# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/bionic64"

  config.vm.provider "virtualbox" do |vb|
    vb.memory = "1024"
    (1..3).each do |i|
        disk = "disk#{i}.vdi"
        unless File.exist?(disk)
          vb.customize ['createhd', '--filename', disk, '--size', 1 * 1024]
        end
        vb.customize ['storageattach', :id, '--storagectl', 'SCSI', '--port', 2+i, '--device', 0, '--type', 'hdd', '--medium', disk]
    end
  end

  config.vm.provision "shell", inline: <<-SHELL
    ifconfig | awk -v hostname="$(hostname)" '/inet / { if ($2 != "127.0.0.1") print $2 " " hostname}' | tee -a /etc/hosts

    apt-add-repository "deb https://download.ceph.com/debian-nautilus/ $(lsb_release -sc) main"
    wget -q -O- 'https://download.ceph.com/keys/release.asc' | sudo apt-key add -

    wget -q https://rebar3.s3.amazonaws.com/rebar3
    cp rebar3 /usr/local/bin
    chmod 755 /usr/local/bin/rebar3

    wget -q https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
    dpkg -i erlang-solutions_1.0_all.deb
    apt-get update
    apt-get install -y erlang librados-dev make gcc ceph-deploy

    ssh-keygen -t rsa -b 2048 -f /root/.ssh/id_rsa -N ""
    cp /root/.ssh/id_rsa /root/.ssh/aurthorized_keys
    mkdir -p /etc/ceph
    cd /etc/ceph
    ceph-deploy new $(hostname) && \
    ceph-deploy install $(hostname) && \
    ceph-deploy mon create-initial && \
    ceph-deploy mgr create $(hostname) && \
    ceph-deploy disk zap $(hostname) /dev/sdc && \
    ceph-deploy disk zap $(hostname) /dev/sdd && \
    ceph-deploy disk zap $(hostname) /dev/sde && \
    ceph-deploy osd create $(hostname) --data /dev/sdc && \
    ceph-deploy osd create $(hostname) --data /dev/sdd && \
    ceph-deploy osd create $(hostname) --data /dev/sde && \
    ceph osd pool create test 1 && \
    ceph osd pool set test size 1 && \
    ceph osd pool set test min_size 1 && \
    ceph osd pool application enable test rbd && \
    ceph -s
  SHELL
end
