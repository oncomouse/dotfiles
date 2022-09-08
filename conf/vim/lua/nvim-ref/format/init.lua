local M = {}
local function buffer_type()
	return vim.opt_local.filetype:get()
end

function M.get_ref(citation)
	local bt = buffer_type()
	if bt == "tex" or bt == "latex" then
		return "\\cite{" .. citation.key .. "}"
	elseif bt == "org" then
		return "[cite:@" .. citation.key .. ";]"
	end
	return "@" .. citation.key
end

function M.get_citation(citation)
	local bt = buffer_type()
	if bt == "tex" or bt == "latex" then
		return {
			before = "\\cite[",
			after = "]{" .. citation.key "}"
		}
	end
	if bt == "org" then
		return {
			before = "[cite:@" .. citation.key .. " ",
			after = ";]"
		}
	end
	return {
		before = "[@" .. citation.key .. ", ",
		after = "]",
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
