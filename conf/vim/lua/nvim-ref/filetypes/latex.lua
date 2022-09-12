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

M.start_pattern = [[\\\w*cite[^{]*{[^, \t}\n]*\%#]]

function M.setup()
	require("nvim-ref.hooks").trigger("add_filetype", {
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
