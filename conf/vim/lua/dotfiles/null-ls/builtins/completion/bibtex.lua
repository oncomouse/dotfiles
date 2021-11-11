-- luacheck: globals vim
-- BibTeX citation completion. This needs iskeyword to match "@", so you have to set
--   vim.opt_local.iskeyword = vim.opt_local.iskeyword + "@-@"
-- or
--   setlocal iskeyword += @-@
-- Somewhere in an ftplugin/markdown.vim file (or with an autocmd)
local h = require("null-ls.helpers")
local methods = require("null-ls.methods")
local bibtex = require("dotfiles.utils.bibtex")

local COMPLETION = methods.internal.COMPLETION

local function make_item(entry_contents)
	return {
		label = entry_contents.key,
		detail = (entry_contents.title or ""),
		documentation = {
			kind = vim.lsp.protocol.MarkupKind.Markdown,
			value = "*Author*: "
				.. (entry_contents.author or "")
				.. "\n"
				.. "*Title*: "
				.. (entry_contents.title or "")
				.. "\n"
				.. "*Year*: "
				.. (entry_contents.date or ""),
		},
		kind = vim.lsp.protocol.CompletionItemKind["Text"],
	}
end

return h.make_builtin({
	method = COMPLETION,
	filetypes = { "markdown", "latex" },
	name = "bibtex",
	generator_opts = {
		runtime_condition = function(params)
			params.bibfiles = vim.g.bibfiles
			return true
		end,
	},
	generator = {
		fn = function(params, done)
			if not string.find(params.word_to_complete, "@", 1, true) then
				done({ { items = {}, isIncomplete = false } })
				return
			end

			local results = bibtex.query_bibtex(params.bibfiles, params.word_to_complete)
			local items = {}
			for _, item in ipairs(results) do
				table.insert(items, make_item(item))
			end
			done({ { items = items, isIncomplete = #items } })
		end,
		async = true,
	},
})
