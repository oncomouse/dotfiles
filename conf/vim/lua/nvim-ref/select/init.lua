local M = {}

local function run_select(input, opts)
	local cb = opts.cb or function() end
	local format = opts.format_item or function(x)
		return x
	end
	local prompt = opts.prompt or "Make a selection: "
	vim.ui.select(input, {
		prompt = prompt,
		format_item = format,
	}, function(choice)
		cb(choice)
	end)
end

function M.citation(cb, query, opts)
	opts = opts or {
		prompt = "Select a citation: ",
	}
	local results = require("nvim-ref.bibliography").query(query or "")
	run_select(results, {
		prompt = opts.prompt,
		format_item = function(item)
			return string.format("@%s: %s - %s [%s]", item.key, item.author, item.title, item.kind)
		end,
		cb = cb,
	})
end

function M.bibliography(cb, opts)
	opts = opts or {
		prompt = "Select a bibilography: ",
	}
	local results = require("nvim-ref.bibliography").bibliographies
	run_select(results, {
		prompt = opts.prompt,
		format_item = function(item)
			local filename = vim.fn.fnamemodify(item, ":p")
			filename = vim.fn.pathshorten(filename)
			return filename
		end,
		cb = cb,
	})
end

return M
