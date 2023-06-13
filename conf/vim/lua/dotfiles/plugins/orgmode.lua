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
			"~/SeaDrive/My Libraries/org/**/*.org",
		},
		org_default_notes_file = "~/SeaDrive/My Libraries/org/inbox.org",
		org_indent_mode = "noindent",
		org_capture_templates = {
			t = { description = "Todo Item", template = "* TODO %?\n  %u" },
			p = { description = "Paste Todo Item", template = "* TODO %x%?\n  %u"},
		},
		mappings = {
			org = {
				org_toggle_checkbox = { "<C-space>", "<leader>o<space>", "gtd" },
			},
		},
	},
	config = function(_, opts)
		require("orgmode").setup_ts_grammar()
		require("orgmode").setup(opts)
		if vim.bo.filetype == "org" then
			require("orgmode").reload(vim.fn.expand("<afile>:p"))
		end
	end,
}
