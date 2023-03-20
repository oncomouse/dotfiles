return {
	"lukas-reineke/indent-blankline.nvim",
	event = "VeryLazy",
	opts = {
		show_end_of_line = true,
		space_char_blankline = " ",
		show_current_context = false,
		filetype_exclude = { "help", "alpha", "dashboard", "neo-tree", "Trouble", "lazy", "mason" },
	},
} -- Mark and highlight indentations
