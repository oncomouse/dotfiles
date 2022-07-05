widget = luastatus.require_plugin("battery-linux").widget({
	cb = function(args)
		local capacity = args.capacity
		local status = args.status
		local icon=""
		local charging="ﮣ"
		if status == "Discharging" then
			charging="ﮤ"
		elseif status == "Charging" then
			icon=""
		elseif status == "Full" then
			capacity = 100
		end
		if capacity ~= nil then
			return string.format("[%s%3.0f%%%s]", icon, capacity, charging)
		end
	end,
})
