packages = [
            'dropbox',
            'emacs24-el',
            'emacs24',
            'python-serial',
            'python-xlib',
            'arduino-mk',
            'arduino',
            'gnome-do',
            'curl',
            'ack-grep',
            'htop',
            'sysstat',
            'dstat',
            'iotop',
            'ant',
            'gcc',
            'g++',
            'libtool',
            'autoconf',
            'automake',
            'cmake',
            'make',
            'zlib1g-dev',
            'libssl-dev',
            'sshfs',
            'libjson0-dev',
            'libfaad-dev',
            'libmad0-dev',
            'libgcrypt11-dev',
            'libgnutls-dev',
            'libao-dev',
            'id3',
            'libxml2-dev',
            'libxslt1-dev',
            'samba',
            'mercurial',
            'xclip',
            'colordiff',
            'evince',
            'mysql-server',
            'mysql-client',
            'libmysqlclient-dev',
            'libsqlite3-dev',
            'openssh-server',
            'acpi',
            'libprotobuf-dev',
            'libncurses5-dev',
            'libio-pty-perl',
            'google-chrome-stable',
            'wireshark',
            'irssi',
            'libyaml-dev',
            'nmap',
            'gnome-disk-utility',
            'imagemagick',
            'git-all',
            'tofrodos',
            'libcurl4-gnutls-dev',
            'apache2',
            'phantomjs',
            'coffeescript',
            'cifs-utils',
            'valgrind',
            'cabal-install',
            'happy',
            'remmina',
            'freetds-dev',
            'bzr',
            'stunnel',
            'wdiff',
            'traceroute',
            'zsh',
            'zsh-doc',
            'iftop',
            'nethogs',
            'markdown',
            'clusterssh',
            'graphviz',
            'openconnect',
            'network-manager-openconnect',
            'gcc-multilib',
            'g++-multilib',
            'gcc-doc',
            'glibc-doc',
            'bash-doc',
            'nasm',
            'gnuplot',
            'gnuplot-doc',
            'tcl-doc',
            'bison',
            'bison-doc',
            'flex',
            'flex-doc',
            'python-wnck',       # used by quicktile
            'cloc',
            'libgoogle-perftools-dev',
            'google-perftools',
            'libavfilter-dev',
            'libavformat-dev',
            'astyle',
            'openjdk-7-jdk',
            'openjdk-7-doc',
            'openjdk-7-source',
            'texlive-latex-base',
            'texlive-fonts-recommended',
            'clang-format-3.8',
            'ant-optional',
            'qemu-kvm', # the next 4 dependencies are for faster android emulation
            'libvirt-bin',
            'ubuntu-vm-builder',
            'bridge-utils',
            'python2.7',
            'npm'
           ]

if node[:development][:lenovo]
  packages << "linux-headers-generic"
  packages << "libbsapi"
  packages << "policykit-1-fingerprint-gui"
  packages << "fingerprint-gui"
end

packages.each { |pkg| package pkg }
