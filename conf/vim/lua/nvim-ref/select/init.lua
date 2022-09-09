local bibtex = require("nvim-ref.utils.bibtex")
local M = {}

M.citation = function(cb, query, opts)
	opts = opts or {
		prompt = "Select a citation"
	}
	local results = require("nvim-ref.bibliography").query(query or "")
	vim.ui.select(results, {
		prompt = opts.prompt,
		format_item = function(item)
			return string.format("@%s: %s - %s [%s]", item.key, item.author, item.title, item.kind)
		end,
	}, function(choice)
		cb(choice)
	end)
end

return M
