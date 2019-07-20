#!/usr/bin/env bash
fish -c "if not functions -q fisher;set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config; curl https://git.io/fisher --create-dirs -sLo \$XDG_CONFIG_HOME/fish/functions/fisher.fish;end; fish -c fisher"
