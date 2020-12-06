function nvim-nightly
  curl -sLO https://github.com/neovim/neovim/releases/download/nightly/nvim-macos.tar.gz
  tar zxvf nvim-macos.tar.gz -C ~/.local/bin/
  rm nvim-macos.tar.gz
end
