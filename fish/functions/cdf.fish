function cdf
  set -l target .
  set -l query ""
  if test (count $argv) -ne 0
    set query -q (string join " " $argv)
  end
  echo $query
  set -l dir (fd $target --type d --color always 2> /dev/null | fzf $query +m --height=40%)
  if test -n "$dir"
    cd $dir
  end
end
