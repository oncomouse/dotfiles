function brew_orphans --description "List brew dependencies"
  set -q argv[1]; and set -l target $argv[1]; or set -l target "all"
  for keg in (if test $target = "all"; brew list -1; else; brew deps $target; end); echo -ne "\x1B[1;34m $keg \x1B[0m"; brew uses $keg --installed | awk '{printf(" %s ", $0)}'; echo ""; end
end
