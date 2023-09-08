# Other Command Aliases:
function cat;bat --paging=never --theme=wal $argv;end
function ls;eval (if command -sq exa; echo "exa"; else; echo "eza"; end) -g --group-directories-first $argv;end
function wal;eval (if command -sq python3; echo "python3"; else; echo "python"; end) $HOME/dotfiles/scripts/wal/custom_wal.py $argv;end
function stow;eval (if command -sq python3; echo "python3"; else; echo "python"; end) $HOME/dotfiles/scripts/stow.py -d "$HOME/dotfiles/stow" -t "$HOME" --no-folding --dotfiles $argv;end
function pilsch.com; ssh eschaton@birkenfeld.dreamhost.com; end
