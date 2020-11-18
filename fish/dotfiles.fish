function __find-all-dots
  set files (fd . ~ --type l -E Projects -E Downloads -E Library -E .asdf -E .Trash -E Applications -H -l | rg dotfiles | sed -e "s/^[lrwx-]* [0-9] [a-z]* *[a-z]* *[0-9]* *[0-9]* *[A-Za-z]* *[0-9]* *[0-9:]* *//g")
  echo "["
  for file in $files;
    echo $file | read -d " -> " link target
    if test -e $target
      echo -n (echo "{"
      echo \"link\": \"$link\",
      echo \"target\": \"$target\"
      echo "}") | string collect -N
    else
      rm $link
      continue
    end
  end | string join ","
  echo "]"
end

function __get-locations -a file
  cat $file | jq '.[] | .link + "," + .target' | sed -e 's/"//g'
end

function __os
  bash $HOME/dotfiles/bootstrap/scripts/os.sh
end
