local M = {}
local function buffer_type()
	return vim.opt_local.filetype:get()
end

function M.get_ref(citation)
	local bt = buffer_type()
	if bt == "tex" or bt == "latex" then
		return require("nvim-ref.filetypes.latex").ref(citation)
	elseif bt == "org" then
		return require("nvim-ref.filetypes.org").ref(citation)
	end
	return require("nvim-ref.filetypes.markdown").ref(citation)
end

function M.get_citation(citation)
	local bt = buffer_type()
	if bt == "tex" or bt == "latex" then
		return require("nvim-ref.filetypes.latex").citation(citation)
	elseif bt == "org" then
		return require("nvim-ref.filetypes.org").citation(citation)
	end
	return require("nvim-ref.filetypes.markdown").citation(citation)
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
