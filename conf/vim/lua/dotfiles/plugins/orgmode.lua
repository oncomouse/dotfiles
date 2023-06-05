return {
	"nvim-orgmode/orgmode",
	ft = "org",
	keys = {
		{ "<leader>oa", desc = "org agenda" },
		{ "<leader>oc", desc = "org capture" },
	},
	opts = {
		org_agenda_files = {
			"~/SeaDrive/My Libraries/Todo/*.org",
			"~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/Associate Head Work/**/*.org",
		}
	},
	config = function(_, opts)
		require("orgmode").setup(opts)
		require("orgmode").setup_ts_grammar()
	end,
}
