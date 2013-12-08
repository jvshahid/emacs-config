ppas = ['cassou/emacs', 'git-core/ppa', 'webupd8team/java',
        'fingerprint/fingerprint-gui']

if node[:development][:lenovo]
  ppas << 'fingerprint/fingerprint-gui'
  ppas << 'bumblebee/stable'
  ppas << 'ubuntu-x-swat/x-updates'
end

def add_ppa ppa
  # search_for=${1/*://}/ubuntu
  # if ! grep -R $search_for /etc/apt/sources.list.d/ > /dev/null 2>&1; then
  #     sudo add-apt-repository $1
  #     sudo apt-get update
  # fi
  bash "ppa:#{ppa}" do
    code <<-EOF
      add-apt-repository -y ppa:#{ppa}
      apt-get -y update
      grep -R #{ppa} /etc/apt/sources.list.d/ > /dev/null 2>&1 || exit 1
    EOF
    not_if "grep -R #{ppa} /etc/apt/sources.list.d/ > /dev/null 2>&1"
  end
end

def add_postgres
  bash "postgres" do
    code <<-EOF
     echo 'deb http://apt.postgresql.org/pub/repos/apt/ precise-pgdg main' > /etc/apt/sources.list.d/pgdg.list
     wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
     apt-get -y update
EOF
  end
end


# TODO: add the pg repo

ppas.each do |ppa|
  add_ppa ppa
end

add_postgres
