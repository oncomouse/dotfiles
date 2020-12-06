function vale-init
  if not test -e ".vale.ini"
    echo "MinAlertLevel = warning # suggestion, warning or error" > .vale.ini
    echo "[*.{md,txt] # Only Markdown and .txt files" >> .vale.ini
    echo "BasedOnStyles = proselint, write-good, Joblint # List of styles to load" >> .vale.ini
    echo "" >> .vale.ini
    echo "# Style.Rule = {YES, NO, suggestion, warning, error} to" >> .vale.ini
    echo "# enable/disable a rule or change its level." >> .vale.ini
  end
end
