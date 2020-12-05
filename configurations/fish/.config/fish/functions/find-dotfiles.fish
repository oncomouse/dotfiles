function find-dotfiles
  set -l dots (__find-all-dots) | string collect -N
  echo $dots > $HOME/dotfiles/locations/(__os).json
end

