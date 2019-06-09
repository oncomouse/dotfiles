function middleman --description "Shorthand for 'bundle exec middleman'"
	set --local middleman_config (eval PWD)/"config.rb"
	if test -e $middleman_config
		eval "bundle exec middleman" $argv
	else
		eval (eval "which middleman") $argv
	end
end