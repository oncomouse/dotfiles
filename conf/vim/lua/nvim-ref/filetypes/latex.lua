local M = {}

function M.ref(citation)
	return "\\cite{" .. citation.key .. "}"
end

function M.citation(citation)
	return {
		before = "\\cite[",
		after = "]{" .. citation.key("}"),
	}
end

function M.find_bibliography(bufnum)
	bufnum = bufnum or 0
end

function M.find_start()
	local curline = vim.fn.line(".")
	local line, col = unpack(vim.fn.searchpos([[\\\w*cite[^{]*{[^, \t}\n]*\%#]], "bcn"))
	print(curline, line, col)
	if line == curline then
		return col
	end
	return nil
end

function M.setup()
	require("nvim-ref.hooks").run_hook("add_filetype", {
		{
			type = "latex",
		},
		{
			type = { "plaintex", "tex" },
			module = "nvim-ref.filetypes.latex",
		},
	})
end

return require("nvim-ref.filetypes.utils").setmetatable(M)
