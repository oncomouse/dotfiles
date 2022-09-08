local M = {}

function M.ref(citation)
	return "[cite:@" .. citation.key .. ";]"
end

function M.citation(citation)
	return {
		before = "[cite:@" .. citation.key .. " ",
		after = ";]",
	}
end

function M.find_bibliography(bufnum)
	bufnum = bufnum or 0
	local lines = require("nvim-ref.utils.file").get_buffer_lines(bufnum)
	local bibliographies = {}
	for _, line in pairs(lines) do
		if string.match(line, "^#+bibliography:") then
			local file = string.gsub(line, "^#+bibliography:%s*", "")
			table.insert(bibliographies, vim.fn.fnamemodify(string.gsub(file, '"', ""), ":p"))
		end
	end
	return bibliographies
end

function M.setup()
	require("nvim-ref.hooks").run_hook("add_filetype", {
		type = "org",
	})
end

return M
