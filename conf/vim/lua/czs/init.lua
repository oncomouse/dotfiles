local M = {}

local hidden = true
local current = nil
local total = nil
local target = nil
local last_target = nil
local searchcount = nil

local function update_searchcount()
	searchcount = vim.fn.searchcount({ recompute = 1 })
end

function M.display_results()
	if vim.fn.mode():sub(1, 1) ~= "n" then
		hidden = true
	end
	target = vim.fn.getreg("/")
	if target ~= last_target then
		last_target = target
		hidden = false
	end
	if not hidden then
		update_searchcount()
		if
			vim.fn.empty(searchcount) == 0
			and searchcount.total ~= 0
			and searchcount.current > 0
		then
			hidden = false
		else
			hidden = true
		end
	end
	return not hidden
end

function M.output()
	if searchcount.incomplete == 1 then -- Timed out
		current = "?"
		total = "??"
	elseif searchcount.incomplete == 2 then -- Max count exceed
		if
			searchcount.total > searchcount.maxcount
			and searchcount.current > searchcount.maxcount
		then
			current = vim.fn.printf(">%d", searchcount.current)
			total = vim.fn.printf(">%d", searchcount.total)
		elseif searchcount.total > searchcount.maxcount then
			current = vim.fn.printf("%d", searchcount.current)
			total = vim.fn.printf(">%d", searchcount.total)
		end
	else
		current = vim.fn.printf("%d", searchcount.current)
		total = vim.fn.printf("%d", searchcount.total)
	end
	return target, current, total
end

local function set_map(move)
	vim.keymap.set("n", move, function()
		hidden = false
		-- Don't move if we have no search results:
		if searchcount.total == 0 then
			return
		end
		pcall(vim.cmd, 'exec "normal! ' .. (vim.v.count == 0 and "" or vim.v.count) .. move .. '"')
	end)
end

function M.init()
	set_map("n")
	set_map("N")
	-- Recaculate search results when entering a buffer:
	vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
		callback = function()
			update_searchcount()
		end,
	})
end

return M
