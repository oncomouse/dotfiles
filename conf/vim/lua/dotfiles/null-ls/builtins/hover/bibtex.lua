-- luacheck: globals vim
-- BibTeX citation hover. This needs iskeyword to match "@", so you have to set
--   vim.opt_local.iskeyword = vim.opt_local.iskeyword + "@-@"
-- or
--   setlocal iskeyword += @-@
-- Somewhere in an ftplugin/markdown.vim file (or with an autocmd)
local h = require("null-ls.helpers")
local methods = require("null-ls.methods")
local bibtex = require("dotfiles.utils.bibtex")

local HOVER = methods.internal.HOVER

local function make_item(entry_contents)
	return "*Author*: "
		.. (entry_contents.author or "")
		.. "\n"
		.. "*Title*: "
		.. (entry_contents.title or "")
		.. "\n"
		.. "*Year*: "
		.. (entry_contents.date or "")
end

return h.make_builtin({
	method = HOVER,
	filetypes = { "markdown" },
	name = "bibtex",
	generator_opts = {
		runtime_condition = function(params)
			params.bibfiles = vim.g.bibfiles
			return true
		end,
	},
	generator = {
		fn = function(params, done)
			local cword = vim.fn.expand("<cword>")

			if not string.find(cword, "@", 1, true) then
				done()
				return
			end
			local results = bibtex.query_bibtex(params.bibfiles, cword)
			if results[1].key == cword then
				done({ make_item(results[1]) })
			else
				done()
			end
		end,
		async = true,
	},
})
