local Lsp = require("lspize")

local function get_documentation(snip, data)
	local header = (snip.name or "") .. " _ `[" .. data.filetype .. "]`\n"
	local docstring = { "", "```" .. vim.bo.filetype, snip:get_docstring(), "```" }
	local documentation = { header .. "---", (snip.dscr or ""), docstring }
	documentation = require("vim.lsp.util").convert_input_to_markdown_lines(documentation)
	return table.concat(documentation, "\n")
end

local handlers = {
	[Lsp.methods.COMPLETION] = function(params, done)
		local curline = vim.fn.line(".")
		local line, col = unpack(vim.fn.searchpos([[\k*]], "bcn"))
		if line ~= curline then
			done()
			return
		end
		local word_to_complete = vim.api.nvim_get_current_line():sub(col - 1, params.position.character)
		local filetypes = require("luasnip.util.util").get_snippet_filetypes()
		local items = {}

		for i = 1, #filetypes do
			local ft = filetypes[i]
			local ft_table = require("luasnip").get_snippets(ft)
			if ft_table then
				for j, snip in pairs(ft_table) do
					local data = {
						type = "luasnip",
						filetype = ft,
						ft_indx = j,
						snip_id = snip.id,
						show_condition = snip.show_condition,
					}
					if not snip.hidden then
						items[#items + 1] = {
							word = snip.trigger,
							label = snip.trigger,
							detail = type(snip.description) == "table" and table.concat(snip.description, "\n") or snip.description,
							kind = vim.lsp.protocol.CompletionItemKind.Snippet,
							data = data,
							documentation = {
								value = get_documentation(snip, data),
								kind = vim.lsp.protocol.MarkupKind.Markdown,
							},
						}
					end
				end
			end
		end
		local line_to_cursor = require("luasnip.util.util").get_current_line_to_cursor()
		items = vim.tbl_filter(function(item)
			return vim.startswith(item.word, word_to_complete) and item.data.show_condition(line_to_cursor)
		end, items)
		done(nil, { isIncomplete = false, items = items })
	end,
}

Lsp.create(handlers, {
	name = "luasnip",
})
