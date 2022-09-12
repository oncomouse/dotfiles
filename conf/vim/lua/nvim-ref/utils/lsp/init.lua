local M = {}

function M.make_lsp_item(entry_contents)
	return {
		label = entry_contents.key,
		detail = (entry_contents.title or ""),
		documentation = {
			kind = vim.lsp.protocol.MarkupKind.Markdown,
			value = M.get_markdown_documentation(entry_contents),
		},
		kind = vim.lsp.protocol.CompletionItemKind["Keyword"],
	}
end

function M.get_markdown_documentation(citation)
	local documentation = {
		"*Author*: " .. (citation.author or ""),
		"*Title*: " .. (citation.title or ""),
		"*Year*: " .. (citation.date or ""),
	}
	documentation = require("vim.lsp.util").convert_input_to_markdown_lines(documentation)
	documentation = table.concat(documentation, "\n")
	return documentation
end

return M
