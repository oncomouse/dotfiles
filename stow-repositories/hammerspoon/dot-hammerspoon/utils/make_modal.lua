-- luacheck: globals hs
local modal_timeout = 10
local function triggerModal(app)
	if type(app) == "function" then
		app()
	else
		hs.application.launchOrFocus(app)
	end
end
function make_modal(mods, hotkey, quickkeys)
	local modal = hs.hotkey.modal.new(mods, hotkey)
	for key, app in pairs(quickkeys) do
		if type(app) == "table" then
			for _k, mod_app in pairs(app) do
				modal:bind(mod_app[1], tostring(key), function()
					triggerModal(mod_app[2])
					modal:exit()
				end)
			end
		else
			modal:bind("", tostring(key), function()
				triggerModal(app)
				modal:exit()
			end)
		end
	end
	modal:bind("", "escape", function()
		modal:exit()
	end)
	-- hyper + a, ? shows list of apps
	modal:bind("shift", "/", function()
		hs.alert.show(hs.inspect.inspect(quickkeys))
		modal:exit()
	end)
	-- Clear modal if nothing happens for 10 seconds:
	function modal:entered()
		hs.timer.doAfter(modal_timeout, function()
			self:exit()
		end)
	end
end
return make_modal
