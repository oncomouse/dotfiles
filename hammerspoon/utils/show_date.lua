-- luacheck: globals hs
function show_date()
	hs.alert.show(os.date([[%B %m, %Y

%H:%M]]), {
		textStyle = {
			paragraphStyle = { alignment = "center" },
		},
	})
end
return show_date
