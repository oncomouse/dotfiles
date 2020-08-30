-- luacheck: globals hs
function make_app_switcher(mods, hotkey, quickkeys)
	local app_switcher = hs.hotkey.modal.new(mods, hotkey)
	for key, app in pairs(quickkeys) do
		if type(app) == "table" then
			for _k, mod_app in pairs(app) do
				app_switcher:bind(mod_app[1], key, function()
					hs.application.launchOrFocus(mod_app[2])
					app_switcher:exit()
				end)
			end
		else
			app_switcher:bind("", key, function()
				hs.application.launchOrFocus(app)
				app_switcher:exit()
			end)
		end
	end
	app_switcher:bind("", "escape", function()
		app_switcher:exit()
	end)
	-- hyper + a, ? shows list of apps
	app_switcher:bind("shift", "/", function()
		hs.alert.show(hs.inspect.inspect(quickkeys))
		app_switcher:exit()
	end)
	function app_switcher:entered()
		hs.timer.doAfter(10, function()
			self:exit()
		end)
	end
	for key, app in pairs(quickkeys) do
		if type(app) == "table" then
			for _k, mod_app in pairs(app) do
				app_switcher:bind(mod_app[1], key, function()
					hs.application.launchOrFocus(mod_app[2])
					app_switcher:exit()
				end)
			end
		else
			app_switcher:bind("", key, function()
				hs.application.launchOrFocus(app)
				app_switcher:exit()
			end)
		end
	end
	app_switcher:bind("", "escape", function()
		app_switcher:exit()
	end)
	-- hyper + a, ? shows list of apps
	app_switcher:bind("shift", "/", function()
		hs.alert.show(hs.inspect.inspect(application_hyperkeys))
		app_switcher:exit()
	end)
	function app_switcher:entered()
		hs.timer.doAfter(10, function()
			self:exit()
		end)
	end
end
return make_app_switcher
