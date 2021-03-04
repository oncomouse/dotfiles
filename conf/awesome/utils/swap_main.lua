local awful = require("awful")
return function(c)
	if c == awful.client.getmaster() then
		awful.client.swap.byidx(1, c)
	else
		c:swap(awful.client.getmaster())
	end
end
