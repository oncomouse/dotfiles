function vale
  set -l query 
  if test (count $argv) -ne 0
    set query (string join " " $argv)
  end
  /usr/local/bin/vale --config "$XDG_CONFIG_HOME/vale/vale.ini" $query
end
