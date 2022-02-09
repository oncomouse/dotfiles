local function truefloat(c)
	return c.floating and not c.maximized and not c.fullscreen
end

return truefloat
