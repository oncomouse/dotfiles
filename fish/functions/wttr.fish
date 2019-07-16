function wttr
  set -l wttr_output (curl -sL "http://wttr.in?format=2")
  if string match -r "^[<>A-Za-z0-9]" $wttr_output
    wego
  else
    echo -n $wttr_output
  end
end
