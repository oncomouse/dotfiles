local bibtex = require("nvim-ref.utils.bibtex")
local M = {}

M.select = function(query)
	local results = bibtex.query_bibtex(require("nvim-ref").config.bibfiles, query or "")
	vim.ui.select(results, {
		prompt = "Digraph: ",
		format_item = function(item)
			return string.format("@%s: %s - %s [%s]", item.key, item.author, item.title, item.kind)
		end,
	}, function(choice)
		print(vim.inspect(choice))
	end)
end

return M
