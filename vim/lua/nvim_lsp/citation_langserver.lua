local configs = require"nvim_lsp/configs"
local util = require"nvim_lsp/util"

local server_name = "citation_langserver"
local bin_name = "citation-langserver"

configs[server_name] = {
	default_config = {
		cmd = { bin_name },
		root_dir = util.root_pattern(".git"),
		filetypes = { "markdown" },
		settings = {
			citation = {
				bibliographies = {},
			},
		},
	},
	docs = {
		description = [[
Citation Language Server for gathering citations from BibTeX files.
]],
		default_config = { root_dir = [[root_pattern(".git")]] },
	},
}
