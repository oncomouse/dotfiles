local org_path = vim.fs.joinpath(vim.g.seadrive_path, "Todo")
return {
	"nvim-orgmode/orgmode",
	dev = false,
	ft = "org",
	keys = {
		{ "<leader>oa", "<Cmd>lua require('orgmode').action('agenda.prompt')<CR>", desc = "org agenda" },
		{ "<leader>oc", "<Cmd>lua require('orgmode').action('capture.prompt')<CR>", desc = "org capture" },
	},
	opts = {
		org_agenda_files = {
			org_path .. "/todo.org",
			org_path .. "/inbox.org",
		},
		org_default_notes_file = org_path .. "/inbox.org",
		org_indent_mode = "noindent",
		org_capture_templates = {
			t = { description = "Todo Item", template = "* TODO %?\n  %u" },
			p = { description = "Paste Todo Item", template = "* TODO %x%?\n  %u" },
		},
		org_tags_column = 0,
		mappings = {
			org = {
				org_timestamp_down_day = { "<S-Left>" },
				org_timestamp_up_day = { "<S-Right>" },
				org_timestamp_up = { "<C-A>", "<S-Up>" },
				org_timestamp_down = { "<C-X>", "<S-Down>" },
				org_toggle_checkbox = { "<prefix><space>", "gtd" },
				org_return = false,
			},
		},
	},
	config = function(_, opts)

		-- Load orgmode patches:
		require("orgmode-patches")

		require("orgmode").setup_ts_grammar()
		require("orgmode").setup(opts)
		if vim.bo.filetype == "org" then
			require("orgmode").reload(vim.fn.expand("<afile>:p"))
		end
	end,
	dependencies = {
		{ "tpope/vim-repeat" }
	}
}
