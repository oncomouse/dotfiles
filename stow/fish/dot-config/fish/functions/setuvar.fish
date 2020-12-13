function setuvar -a var_name var_value --description "Set a universal variable if it is not already set"
  if not set -q -U $var_name
    echo "Setting $var_name"
    set -Ux $var_name $var_value
    return 1
  end
  return 0
end
