#function bower --description "Check if bower is in local node_modules"
#	set --local bower (eval PWD)/"node_modules/bower/bin/bower"
#	if test -e $bower
#		eval $bower $argv
#	else
#		set_color $fish_color_normal --bold; echo "Bower not found in ./node_modules/bower"
#		echo ""
#		set_color $fish_color_normal; echo -n "Run: "
#		set_color $fish_color_command; echo "npm install bower"
#	end
#end