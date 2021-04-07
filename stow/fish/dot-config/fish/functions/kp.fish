# Source: https://github.com/SidOfc/dotfiles/blob/master/config.fish
function kp --description "Kill processes"
  set -l __kp__pid ''
  set __kp__pid (ps -ef | sed 1d | eval "fzf $FZF_DEFAULT_OPTS -m --header='[kill:process]' --reverse" | awk '{print $2}')

  if test "x$__kp__pid" != "x"
    if test "x$argv[1]" != "x"
      echo $__kp__pid | xargs kill $argv[1]
    else
      echo $__kp__pid | xargs kill
    end
    # exec kp
  end
end

