local ok = require("fzf-lua")
if ok then
	local wh = vim.api.nvim_win_get_height(0)
	local ph = vim.o.previewheight
	require("fzf-lua").setup({
		winopts = {
			height = ph / wh,
			width = 1.0,
			row = 0.99,
			col = 0,
			border = "none",
			preview = {
				flip_columns = 110,
			},
		},
		files = {
			actions = {
				["default"] = require("fzf-lua.actions").file_edit,
			},
		},
		git = {
			status = {
				actions = {
					["default"] = function()
						vim.cmd("Gina commit")
					end,
				},
			},
		},
		fzf_colors = {
			["fg"] = { "fg", "Normal" },
			["bg"] = { "bg", "Normal" },
			["hl"] = { "fg", "Comment" },
			["fg+"] = { "fg", "Normal" },
			["bg+"] = { "bg", "CursorLine" },
			["hl+"] = { "fg", "Statement" },
			["info"] = { "fg", "PreProc" },
			["prompt"] = { "fg", "Label" },
			["pointer"] = { "fg", "Identifier" },
			["marker"] = { "fg", "Identifier" },
			["spinner"] = { "fg", "Identiier" },
			["header"] = { "fg", "Comment" },
			["gutter"] = { "bg", "Normal" },
		},
	})
	require("fzf-lua").register_ui_select()
	vim.api.nvim_create_user_command("Files", function(args)
		require("fzf-lua").files({
			fzf_opts = { ["--layout"] = "reverse-list", ["--info"] = "inline" },
			cwd = args.args == "" and "." or args.args,
		})
	end, {
		complete = "dir",
		force = true,
		nargs = "?",
	})
	vim.api.nvim_create_user_command("Buffers", function()
		require("fzf-lua").buffers()
	end, {
		force = true,
	})
	vim.api.nvim_create_user_command("GitStatus", function()
		require("fzf-lua").git_status()
	end, {
		force = true,
	})
end
