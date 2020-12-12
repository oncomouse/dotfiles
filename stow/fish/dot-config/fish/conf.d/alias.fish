# Universal ignore for ag
function ag; /usr/bin/env ag --path-to-ignore ~/.ignore --hidden $argv; end
# Other Command Aliases:
function cat;bat --paging=never --theme=wal $argv;end
function icat;kitty +kitten icat $argv; end
function top;htop $argv;end
function ls;exa --group-directories-first $argv;end
function wal;eval (if command -sq python3; echo "python3"; else; echo "python"; end) $HOME/dotfiles/scripts/wal/custom_wal.py $argv;end
function stow;eval (if command -sq python3; echo "python3"; else; echo "python"; end) $HOME/dotfiles/scripts/stow.py -d "$HOME/dotfiles/stow" -t "$HOME" --no-folding --dotfiles $argv;end
# Vim is Neovim in server mode:
function vim
  if command -sq nvim
    eval (which nvim) -u ~/.vimrc $argv
  else
    eval (which vim) $argv
  end
end
# Vi is Neovim or Vim in barebones mode:
function vi
  if command -sq nvim
    eval (which nvim) -u ~/dotfiles/conf/vim/vimrc-minimal $argv
  else if command -sq vim
    eval (which vim) -u ~/dotfiles/conf/vim/vimrc-minimal $argv
  else
    eval (which vi) $argv
  end
end
function standard;~/.npm-packages/bin/semistandard $argv | ~/.npm-packages/bin/snazzy;end
function janet-repl;/usr/local/bin/janet -e "(import spork/netrepl) (netrepl/server)";end
function nvim5;$HOME/.local/bin/nvim-osx64/bin/nvim $argv;end

