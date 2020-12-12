# Local paths:
function add_to_user_paths -a dir
  # Delete $dir if it is present but does not exist:
  if set -l index (contains -i $dir $fish_user_paths)
    if not test -d $dir
      set -e -U fish_user_paths[$index]
    end
  # Add $dir if it is not present but does exist:
  else
    if test -d $dir
      echo "Adding" $dir "to fish_user_paths"
      set -Ux fish_user_paths $fish_user_paths $dir
    end
  end
end

