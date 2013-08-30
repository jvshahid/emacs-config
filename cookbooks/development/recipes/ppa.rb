ppas = ['cassou/emacs', 'git-core/ppa', 'webupd8team/java', 'sun-java-community-team/sun-java6',
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
    EOF
    not_if "grep -R #{ppa} /etc/apt/sources.list.d/ > /dev/null 2>&1"
  end
end


# TODO: add the pg repo

ppas.each do |ppa|
  add_ppa ppa
end
