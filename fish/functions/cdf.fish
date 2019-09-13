function cdf
  set -l target .
  if test (count $argv) -ne 0; set target $argv[1]; end
  set -l dir (fd $target --type d --color always 2> /dev/null | fzf +m --height=40%)
  if test -n "$dir"
    cd $dir
  end
end
