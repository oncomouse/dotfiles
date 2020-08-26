function reload_config(files)
	local doReload = false
	for _, file in pairs(files) do
		if file:sub(-4) == ".lua" then
			doReload = true
			break
		end
	end
	if doReload then
		-- stop watchers to avoid leaks
		hs.fnutils.each(ext.watchers, function(watcher)
			watcher:stop()
		end)

		hs.reload()
	end
end
return reload_config
