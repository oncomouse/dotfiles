function cdf
  set -l target .
  set -l query 
  if test (count $argv) -ne 0
    set query -q (string join " " $argv)
  end
  set -l dir (fd $target --type d 2> /dev/null | fzf $query +m --layout=reverse --height=40% --preview='ls -1 --color=always {} | head -n 10')
  if test -n "$dir"
    cd $dir
  end
end
