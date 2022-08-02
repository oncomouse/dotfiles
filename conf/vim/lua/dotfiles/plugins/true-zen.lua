true_zen = {}

function true_zen.narrow_opfunc(type)
	if type == nil then
		if vim.b.tz_narrowed_buffer then
			return ":TZNarrow<cr>"
		end
		vim.opt.opfunc = "v:lua.true_zen.narrow_opfunc"
		return "g@"
	end
	require("true-zen.narrow").toggle(vim.fn.getpos("'[")[2], vim.fn.getpos("']")[2])
end

-- local augroup = vim.api.nvim_create_augroup("dotfiles-settings-true-zen", {})

local function config_true_zen()
	local bufnr = nil
	require("true-zen").setup({
		modes = {
			ataraxis = {
				minimum_writing_area = {
					width = 80,
				},
				-- open_callback = function()
				-- 	vim.api.nvim_create_autocmd("QuitPre", {
				-- 		group = augroup,
				-- 		callback = function()
				-- 			if vim.b.tz_narrowed_buffer then
				-- 				vim.schedule(function()
				-- 					vim.cmd([[normal! zE]])
				-- 				end)
				-- 				vim.b.tz_narrowed_buffer = nil
				-- 			end
				-- 		end,
				-- 	})
				-- end,
				-- close_callback = function()
				-- 	augroup = vim.api.nvim_create_augroup("dotfiles-settings-true-zen", { clear = true, })
				-- end,
			},
			minimalist = {
				open_callback = function()
					bufnr = vim.fn.bufnr("0")
					vim.keymap.set("n", "z=", "<cmd>FzfLua spell_suggest<CR>", { silent = true, buffer = bufnr })
				end,
				close_callback = function()
					-- For some reason this both works and causes an error, so we have to wrap it in pcall:
					pcall(vim.keymap.del, "n", "z=", { buffer = bufnr })
				end,
			},
			narrow = {
				folds_style = "invisible",
			},
			focus = {
				open_callback = function()
					vim.opt_local.showtabline = 0
				end,
				close_callback = function()
					vim.opt_local.showtabline = 1
				end,
			},
		},
	})
	vim.keymap.set("n", "gz", function()
		return true_zen.narrow_opfunc()
	end, {
		expr = true,
	})
	vim.keymap.set("v", "gz", "<cmd>'<,'>TZNarrow<CR>")
	vim.keymap.set("n", "<C-W>z", "<cmd>TZFocus<CR>")
	vim.keymap.set("n", "<leader>z", "<cmd>TZAtaraxis<cr>")
end

return config_true_zen
