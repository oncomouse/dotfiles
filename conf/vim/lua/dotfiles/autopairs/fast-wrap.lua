local npairs = require("nvim-autopairs")
npairs.setup({
	fast_wrap = {
		map = "<C-e>",
		chars = { "{", "[", "(", '"', "'", "*", "_" },
	},
})
