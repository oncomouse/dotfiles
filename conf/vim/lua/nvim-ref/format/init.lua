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

return M
