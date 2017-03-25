echo "export PATH=\"${HOME}/.cabal/bin:${PATH}\"" >> "${HOME}/.bashrc"
cd /vagrant
cabal update
cabal install # --enable-tests
