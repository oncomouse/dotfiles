widget = luastatus.require_plugin("file-contents-linux").widget({
	filename = "/tmp/sf-vscreen",
	cb = function(fp)
		if fp then
			local lines = fp:read("*a")
			return string.format("[ %d]", tonumber(lines) + 1)
		end
		return ""
	end
})
