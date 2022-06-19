-- From heirline.nvim (https://github.com/rebelot/heirline.nvim)
local M = {}

function M.width_percent_below(n, thresh, is_winbar)
	local winwidth
	if vim.o.laststatus == 3 and not is_winbar then
		winwidth = vim.o.columns
	else
		winwidth = vim.api.nvim_win_get_width(0)
	end

	return n / winwidth <= thresh
end

local function pattern_list_match(str, pattern_list)
    for _, pattern in ipairs(pattern_list) do
        if str:find(pattern) then
            return true
        end
    end
    return false
end

local buf_matchers = {
    filetype = function(pattern_list)
        local ft = vim.bo.filetype
        return pattern_list_match(ft, pattern_list)
    end,
    buftype = function(pattern_list)
        local bt = vim.bo.buftype
        return pattern_list_match(bt, pattern_list)
    end,
    bufname = function(pattern_list)
        local bn = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":t")
        return pattern_list_match(bn, pattern_list)
    end,
}


function M.buffer_matches(patterns)
    for kind, pattern_list in pairs(patterns) do
        if buf_matchers[kind](pattern_list) then
            return true
        end
    end
    return false
end

return M
