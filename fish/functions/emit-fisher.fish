function emit-fisher
  # Install fisher if it isn't present:
  if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
  end

  # Overwrite the fisher function to add events:
  functions --copy fisher __official_fisher

  # Custom fisher function that emits events:
  function fisher
    __official_fisher $argv
    if [ $status -eq 0 ]
      if test -z $argv[1]
        emit "fisher_install"
      else
        switch $argv[1]
          case "add"
            emit "fisher_add" $argv[2..-1]
          case "ls"
            emit "fisher_ls" $argv[2..-1]
          case "rm"
            emit "fisher_rm" $argv[2..-1]
          case "self-update"
            emit "fisher_self-update" $argv[2..1]
        end
      end
    end
  end
end
