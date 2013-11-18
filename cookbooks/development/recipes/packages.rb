packages = [
            'emacs-snapshot-el',
            'emacs-snapshot-gtk',
            'emacs-snapshot',
            'gnome-do',
            'curl',
            'ack-grep',
            'htop',
            'sysstat',
            'dstat',
            'iotop',
            'ant',
            'unrar',
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
            'chmsee',
            'ghc',
            'ghc-doc',
            'skype',
            'postgresql',
            'postgresql-client',
            'postgresql-server-dev-all',
            'postgresql-contrib',
            'google-chrome-stable',
            'wireshark',
            'irssi',
            'libyaml-dev',
            'nmap',
            'openjdk-7-source',
            'openjdk-6-source',
            'gnome-disk-utility',
            'imagemagick',
            'git-core',
            'git-svn',
            'git-gui',
            'tofrodos',
            'libcurl4-gnutls-dev',
            'apache2',
            'phantomjs',
            'coffeescript',
            'oracle-java7-installer',
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
            'openjdk-7-jdk',
            'openjdk-6-jdk',
            'gcc-multilib',
            'g++-multilib',
            'gcc-doc',
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
            'python3.3'
           ]

if node[:development][:lenovo]
  packages << "linux-headers-generic"
  packages << "libbsapi"
  packages << "policykit-1-fingerprint-gui"
  packages << "fingerprint-gui"
end

packages.each { |pkg| package pkg }
