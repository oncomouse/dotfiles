-- BibTeX citation hover. This needs iskeyword to match "@", so you have to set
--   vim.opt_local.iskeyword = vim.opt_local.iskeyword + "@-@"
-- or
--   setlocal iskeyword += @-@
-- Somewhere in an ftplugin/markdown.vim file (or with an autocmd)
local h = require("null-ls.helpers")
local methods = require("null-ls.methods")
local bibtex = require("nvim-ref.utils.bibtex")

local HOVER = methods.internal.HOVER

return h.make_builtin({
	method = HOVER,
	filetypes = { "markdown" },
	name = "bibtex",
	generator = {
		fn = function(_, done)
			local cword = vim.fn.expand("<cword>")

			if string.match(cword, "^@") then
				cword = string.sub(cword, 2)
			end
			local results = bibtex.query_bibtex(require("nvim-ref").config.bibfiles, cword)
			if results[1].key == cword then
				done({ require("nvim-ref.format").get_markdown_documentation(results[1]) })
			else
				done()
			end
		end,
		async = true,
	},
})
