widget = {
	plugin = 'timer',
	opts = {
		period = 15
	},
	cb = function()
		return os.date("[%a %m/%d %I:%M %p]")
	end,
}
