-- Client mouse behavior
client.connect_signal("mouse::enter", function(c) -- Sloppy focus
	c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)

