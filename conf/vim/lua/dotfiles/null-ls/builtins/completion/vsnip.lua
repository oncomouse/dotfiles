local h = require("null-ls.helpers")
local methods = require("null-ls.methods")

local COMPLETION = methods.internal.COMPLETION

return h.make_builtin({
	method = COMPLETION,
	filetypes = {},
	name = "vsnip",
	generator = {
		fn = function(params, done)
			local items = {}
			local snips = vim.fn["vsnip#get_complete_items"](params.bufnr)
			local targets = vim.tbl_filter(function(item)
				return string.match(item.word, "^" .. params.word_to_complete)
			end, snips)
			for _, item in ipairs(targets) do
				local user_data = vim.fn.json_decode(item.user_data)
				local documentation = {}
				table.insert(documentation, string.format("```%s", params.ft))
				for _, line in ipairs(vim.split(vim.fn["vsnip#to_string"](user_data.vsnip.snippet), "\n")) do
					table.insert(documentation, line)
				end
				table.insert(documentation, "```")
				table.insert(items, {
					label = item.abbr,
					detail = item.menu,
					kind = vim.lsp.protocol.CompletionItemKind["Snippet"],
					documentation = {
						value = table.concat(documentation, '\n'),
						kind = vim.lsp.protocol.MarkupKind.Markdown,
					},
				})
			end
			done({ { items = items, isIncomplete = #items == 0 } })
		end,
		async = true,
	},
})
