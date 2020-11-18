function move-dotfiles
  set -l os (__os)
  set -l locations (__get-locations $HOME/dotfiles/locations/$os.json)
  set -l updates (echo $argv | jq '[] | .from + "," + .to' | sed -e 's/"//g')
  for u in $updates
    echo $u | read -d "," from to
    for l in $locations
      echo $l | read -d "," link target
      if $from = $target
        rm $link
        ln -sf $link $to
      end
    end
  end
  # Generate new location file:
  find-dotfiles
end
