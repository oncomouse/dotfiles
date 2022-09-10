-- BibTeX citation completion. This needs iskeyword to match "@", so you have to set
--   vim.opt_local.iskeyword = vim.opt_local.iskeyword + "@-@"
-- or
--   setlocal iskeyword += @-@
-- Somewhere in an ftplugin/markdown.vim file (or with an autocmd)
local h = require("null-ls.helpers")
local methods = require("null-ls.methods")
local bibtex = require("nvim-ref.utils.bibtex")

local COMPLETION = methods.internal.COMPLETION

local function make_item(entry_contents)
	return {
		label = entry_contents.key,
		detail = (entry_contents.title or ""),
		documentation = {
			kind = vim.lsp.protocol.MarkupKind.Markdown,
			value = require("nvim-ref.format").get_markdown_documentation(entry_contents),
		},
		kind = vim.lsp.protocol.CompletionItemKind["Keyword"],
	}
end

return h.make_builtin({
	method = COMPLETION,
	filetypes = vim.tbl_keys(require("nvim-ref").filetypes),
	name = "bibtex",
	generator = {
		fn = function(params, done)
			if require("nvim-ref.filetypes").require().find_start() == nil then
				done({ { items = {}, isIncomplete = false } })
				return
			end

			local results = require("nvim-ref.bibliography").query(params.word_to_complete)

			local items = {}
			for _, item in ipairs(results) do
				table.insert(items, make_item(item))
			end
			done({ { items = items, isIncomplete = #items } })
		end,
		async = true,
	},
})
