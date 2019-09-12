function cdf
  set -l target .
  if test (count $argv) -ne 0; set target $argv[1]; end
  cd (fd $target --type d --color always 2> /dev/null | fzf +m)
end
