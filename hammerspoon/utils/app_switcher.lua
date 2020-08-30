-- luacheck: globals hs
local modal_timeout = 10
local function launchApp(app)
	if type(app) == "function" then
		app()
	else
		hs.application.launchOrFocus(app)
	end
end
function make_app_switcher(mods, hotkey, quickkeys)
	local app_switcher = hs.hotkey.modal.new(mods, hotkey)
	for key, app in pairs(quickkeys) do
		if type(app) == "table" then
			for _k, mod_app in pairs(app) do
				app_switcher:bind(mod_app[1], tostring(key), function()
					launchApp(mod_app[2])
					app_switcher:exit()
				end)
			end
		else
			app_switcher:bind("", tostring(key), function()
				launchApp(app)
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
	-- Clear modal if nothing happens for 10 seconds:
	function app_switcher:entered()
		hs.timer.doAfter(modal_timeout, function()
			self:exit()
		end)
	end
end
return make_app_switcher
