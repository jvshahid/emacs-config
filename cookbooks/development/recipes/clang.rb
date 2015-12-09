bash "add clang ppa" do
  code <<-EOF
    wget -O - http://llvm.org/apt/llvm-snapshot.gpg.key|sudo apt-key add -
    sudo sh -c 'echo deb http://llvm.org/apt/trusty/ llvm-toolchain-trusty main' > /etc/apt/sources.list.d/clang.list
    sudo sh -c 'echo deb http://llvm.org/apt/trusty/ llvm-toolchain-trusty-3.7 main' >> /etc/apt/sources.list.d/clang.list
  EOF
  not_if "grep -R llvm-toolchain-trusty /etc/apt/sources.list.d/ > /dev/null 2>&1"
end
