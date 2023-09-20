return {
	{
		"gpanders/nvim-parinfer",
		ft = {
			"clojure",
			"scheme",
			"lisp",
			"racket",
			"hy",
			"fennel",
			"janet",
			"carp",
			"wast",
			"yuck",
			"dune",
		},
		config = function()
			vim.fn["parinfer#init"]()
		end
	},
}
