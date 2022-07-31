true_zen = {}

function true_zen.narrow_opfunc(type)
	if type == nil then
		if vim.b.tz_narrowed_buffer then
			return ":TZNarrow<cr>"
		end
		vim.opt.opfunc = "v:lua.true_zen.narrow_opfunc"
		return "g@"
	end
	require("true-zen.narrow").toggle(vim.fn.getpos("'[")[2],vim.fn.getpos("']")[2])
end

local function config_true_zen()
	require("true-zen").setup({
		modes = {
			ataraxis = {
				minimum_writing_area = {
					width = 80,
				},
			},
			narrow = {
				folds_style = "invisible",
			},
			focus = {
				open_callback = function() vim.opt_local.showtabline = 0 end,
				close_callback = function() vim.opt_local.showtabline = 1 end,
			}
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
