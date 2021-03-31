local ts_configs = require('nvim-treesitter.configs')

ts_configs.setup{
	ensure_installed = "maintained",
	highlight = {
		enable = true,
	},
	indent = {
		enable = false,
	},
	textobjects = {
	select = {
		enable = true,
		keymaps = {
			-- You can use the capture groups defined in textobjects.scm
			["af"] = "@function.outer",
			["if"] = "@function.inner",
			["ac"] = "@class.outer",
			["ic"] = "@class.inner",
			},
		},
	},
	move = {
		enable = true,
		goto_next_start = {
			["]m"] = "@function.outer",
			["]]"] = "@class.outer",
		},
		goto_next_end = {
			["]M"] = "@function.outer",
			["]["] = "@class.outer",
		},
		goto_previous_start = {
			["[m"] = "@function.outer",
			["[["] = "@class.outer",
		},
		goto_previous_end = {
			["[M"] = "@function.outer",
			["[]"] = "@class.outer",
		},
	},
	lsp_interop = {
		enable = true,
		peek_definition_code = {
			["df"] = "@function.outer",
			["dF"] = "@class.outer",
		},
	},
	autotag = {
		enable = true,
	}
}
