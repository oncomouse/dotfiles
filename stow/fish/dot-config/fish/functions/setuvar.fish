function setuvar -a var_name var_value
  if not set -q -U "$var_name"
    echo "Setting $var_name"
    set -Ux "$var_name" "$var_value"
    return 1
  end
  return 0
end
