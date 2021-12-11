local h = require("null-ls.helpers")
local methods = require("null-ls.methods")

local COMPLETION = methods.internal.COMPLETION

return h.make_builtin({
	method = COMPLETION,
	filetypes = {},
	name = "vsnip",
	generator = {
		fn = function(params, done)
			print(vim.inspect(params))
			local items = {}
			local snips = vim.fn["vsnip#get_complete_items"](params.bufnr)
			for _, item in ipairs(snips) do
				local user_data = vim.fn.json_decode(item.user_data)

				local snippet = table.concat(user_data.vsnip.snippet, "\n")
				local textEdit = {
					range = {
						start = {
							line = params.row,
							character = params.col,
						},
						["end"] = {
							line = params.row,
							character = params.col,
						},
					},
					newText = snippet,
				}
				print(vim.inspect(textEdit))

				-- Extract snippet description (adapted from cmp-vsnip):
				local documentation = {}
				table.insert(documentation, string.format("```%s", params.ft))
				for _, line in ipairs(vim.split(vim.fn["vsnip#to_string"](user_data.vsnip.snippet), "\n")) do
					table.insert(documentation, line)
				end
				table.insert(documentation, "```")
				documentation = vim.lsp.util.convert_input_to_markdown_lines(documentation)
				documentation = table.concat(documentation, "\n")

				table.insert(items, {
					label = item.abbr,
					detail = item.menu,
					kind = vim.lsp.protocol.CompletionItemKind.Snippet,
					textEdit = textEdit,
					insertTextFormat = vim.lsp.protocol.InsertTextFormat.Snippet,
					documentation = {
						value = documentation,
						kind = vim.lsp.protocol.MarkupKind.Markdown,
					},
				})
			end
			done({ { items = items, isIncomplete = #items == 0 } })
		end,
		async = true,
	},
})
