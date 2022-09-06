local bibtex = require("nvim-ref.utils.bibtex")
local M = {}

M.insert = function(query, args)
	args = args or {
		type = "key"
	}
	local results = bibtex.query_bibtex(require("nvim-ref").config.bibfiles, query or "")
	vim.ui.select(results, {
		prompt = "Digraph: ",
		format_item = function(item)
			return string.format("@%s: %s - %s [%s]", item.key, item.author, item.title, item.kind)
		end,
	}, function(choice)
		require("nvim-ref.insert")[args.type](choice)
	end)
end

return M
