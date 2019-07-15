#!/usr/bin/env bash

if [ ! -f ~/.config/fish/functions/fisher.fish ]; then
  curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
  fish -c "fisher add jethrokuan/fzf fishgretel/fasd excitedleigh/virtualfish wk/plugin-ssh-term-helper"
fi

echo '
function __fasd_run -e fish_preexec -d "fasd takes record of the directories changed into"
  if test $argv[1] != "exit"
      command fasd --proc (command fasd --sanitize "$argv") > "/dev/null" 2>&1 &
  end
end' > ~/.config/fish/conf.d/__fasd_run.fish
