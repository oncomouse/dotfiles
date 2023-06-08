return {
	"nvim-orgmode/orgmode",
	ft = "org",
	keys = {
		{ "<leader>oa", "<Cmd>lua require('orgmode').action('agenda.prompt')<CR>", desc = "org agenda" },
		{ "<leader>oc", "<Cmd>lua require('orgmode').action('capture.prompt')<CR>", desc = "org capture" },
	},
	opts = {
		org_agenda_files = {
			"~/SeaDrive/My Libraries/Todo/*.org",
			"~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/Associate Head Work/**/*.org",
		},
		org_indent_mode = "noindent",
	},
	config = function(_, opts)
		require("orgmode").setup_ts_grammar()
		require("orgmode").setup(opts)
		if vim.bo.filetype == "org" then
			require("orgmode").reload(vim.fn.expand("<afile>:p"))
		end
	end,
}
