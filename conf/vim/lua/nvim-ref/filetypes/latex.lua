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

function M.setup()
	require("nvim-ref.hooks").run_hook("add_filetype", {
		type = "latex",
	})
end

return M
