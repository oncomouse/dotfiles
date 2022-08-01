complete -c starship -n "__fish_use_subcommand" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_use_subcommand" -s V -l version -d 'Print version information'
complete -c starship -n "__fish_use_subcommand" -f -a "bug-report" -d 'Create a pre-populated GitHub issue with information about your configuration'
complete -c starship -n "__fish_use_subcommand" -f -a "completions" -d 'Generate starship shell completions for your shell to stdout'
complete -c starship -n "__fish_use_subcommand" -f -a "config" -d 'Edit the starship configuration'
complete -c starship -n "__fish_use_subcommand" -f -a "explain" -d 'Explains the currently showing modules'
complete -c starship -n "__fish_use_subcommand" -f -a "init" -d 'Prints the shell function used to execute starship'
complete -c starship -n "__fish_use_subcommand" -f -a "module" -d 'Prints a specific prompt module'
complete -c starship -n "__fish_use_subcommand" -f -a "print-config" -d 'Prints the computed starship configuration'
complete -c starship -n "__fish_use_subcommand" -f -a "prompt" -d 'Prints the full starship prompt'
complete -c starship -n "__fish_use_subcommand" -f -a "session" -d 'Generate random session key'
complete -c starship -n "__fish_use_subcommand" -f -a "time" -d 'Prints time in milliseconds'
complete -c starship -n "__fish_use_subcommand" -f -a "timings" -d 'Prints timings of all active modules'
complete -c starship -n "__fish_use_subcommand" -f -a "toggle" -d 'Toggle a given starship module'
complete -c starship -n "__fish_use_subcommand" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c starship -n "__fish_seen_subcommand_from bug-report" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from completions" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from config" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from explain" -s s -l status -d 'The status code of the previously run command as an unsigned or signed 32bit integer' -r
complete -c starship -n "__fish_seen_subcommand_from explain" -l pipestatus -d 'Bash, Fish and Zsh support returning codes for each process in a pipeline' -r
complete -c starship -n "__fish_seen_subcommand_from explain" -s w -l terminal-width -d 'The width of the current interactive terminal' -r
complete -c starship -n "__fish_seen_subcommand_from explain" -s p -l path -d 'The path that the prompt should render for' -r -F
complete -c starship -n "__fish_seen_subcommand_from explain" -s P -l logical-path -d 'The logical path that the prompt should render for. This path should be a virtual/logical representation of the PATH argument' -r -F
complete -c starship -n "__fish_seen_subcommand_from explain" -s d -l cmd-duration -d 'The execution duration of the last command, in milliseconds' -r
complete -c starship -n "__fish_seen_subcommand_from explain" -s k -l keymap -d 'The keymap of fish/zsh/cmd' -r
complete -c starship -n "__fish_seen_subcommand_from explain" -s j -l jobs -d 'The number of currently running jobs' -r
complete -c starship -n "__fish_seen_subcommand_from explain" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from init" -l print-full-init
complete -c starship -n "__fish_seen_subcommand_from init" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from module" -s s -l status -d 'The status code of the previously run command as an unsigned or signed 32bit integer' -r
complete -c starship -n "__fish_seen_subcommand_from module" -l pipestatus -d 'Bash, Fish and Zsh support returning codes for each process in a pipeline' -r
complete -c starship -n "__fish_seen_subcommand_from module" -s w -l terminal-width -d 'The width of the current interactive terminal' -r
complete -c starship -n "__fish_seen_subcommand_from module" -s p -l path -d 'The path that the prompt should render for' -r -F
complete -c starship -n "__fish_seen_subcommand_from module" -s P -l logical-path -d 'The logical path that the prompt should render for. This path should be a virtual/logical representation of the PATH argument' -r -F
complete -c starship -n "__fish_seen_subcommand_from module" -s d -l cmd-duration -d 'The execution duration of the last command, in milliseconds' -r
complete -c starship -n "__fish_seen_subcommand_from module" -s k -l keymap -d 'The keymap of fish/zsh/cmd' -r
complete -c starship -n "__fish_seen_subcommand_from module" -s j -l jobs -d 'The number of currently running jobs' -r
complete -c starship -n "__fish_seen_subcommand_from module" -s l -l list -d 'List out all supported modules'
complete -c starship -n "__fish_seen_subcommand_from module" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from print-config" -s d -l default -d 'Print the default instead of the computed config'
complete -c starship -n "__fish_seen_subcommand_from print-config" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from prompt" -s s -l status -d 'The status code of the previously run command as an unsigned or signed 32bit integer' -r
complete -c starship -n "__fish_seen_subcommand_from prompt" -l pipestatus -d 'Bash, Fish and Zsh support returning codes for each process in a pipeline' -r
complete -c starship -n "__fish_seen_subcommand_from prompt" -s w -l terminal-width -d 'The width of the current interactive terminal' -r
complete -c starship -n "__fish_seen_subcommand_from prompt" -s p -l path -d 'The path that the prompt should render for' -r -F
complete -c starship -n "__fish_seen_subcommand_from prompt" -s P -l logical-path -d 'The logical path that the prompt should render for. This path should be a virtual/logical representation of the PATH argument' -r -F
complete -c starship -n "__fish_seen_subcommand_from prompt" -s d -l cmd-duration -d 'The execution duration of the last command, in milliseconds' -r
complete -c starship -n "__fish_seen_subcommand_from prompt" -s k -l keymap -d 'The keymap of fish/zsh/cmd' -r
complete -c starship -n "__fish_seen_subcommand_from prompt" -s j -l jobs -d 'The number of currently running jobs' -r
complete -c starship -n "__fish_seen_subcommand_from prompt" -l right -d 'Print the right prompt (instead of the standard left prompt)'
complete -c starship -n "__fish_seen_subcommand_from prompt" -l continuation -d 'Print the continuation prompt (instead of the standard left prompt)'
complete -c starship -n "__fish_seen_subcommand_from prompt" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from session" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from time" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from timings" -s s -l status -d 'The status code of the previously run command as an unsigned or signed 32bit integer' -r
complete -c starship -n "__fish_seen_subcommand_from timings" -l pipestatus -d 'Bash, Fish and Zsh support returning codes for each process in a pipeline' -r
complete -c starship -n "__fish_seen_subcommand_from timings" -s w -l terminal-width -d 'The width of the current interactive terminal' -r
complete -c starship -n "__fish_seen_subcommand_from timings" -s p -l path -d 'The path that the prompt should render for' -r -F
complete -c starship -n "__fish_seen_subcommand_from timings" -s P -l logical-path -d 'The logical path that the prompt should render for. This path should be a virtual/logical representation of the PATH argument' -r -F
complete -c starship -n "__fish_seen_subcommand_from timings" -s d -l cmd-duration -d 'The execution duration of the last command, in milliseconds' -r
complete -c starship -n "__fish_seen_subcommand_from timings" -s k -l keymap -d 'The keymap of fish/zsh/cmd' -r
complete -c starship -n "__fish_seen_subcommand_from timings" -s j -l jobs -d 'The number of currently running jobs' -r
complete -c starship -n "__fish_seen_subcommand_from timings" -s h -l help -d 'Print help information'
complete -c starship -n "__fish_seen_subcommand_from toggle" -s h -l help -d 'Print help information'
