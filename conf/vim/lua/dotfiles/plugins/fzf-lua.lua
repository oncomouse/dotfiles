return {
	"ibhagwan/fzf-lua",
	keys = {
		{ mode = "n", "<C-p>", "<Plug>(dotfiles-fzf-files)" },
		{ mode = "n", "<Leader>a", "<Plug>(dotfiles-fzf-buffers)" },
	},
	cmd = { "Files", "Buffers", "FzfLua" },
	init = function() -- Shim vim.ui.select until we can load the plugin
		vim.ui.select = function(...)
			require("fzf-lua.providers.ui_select").ui_select(...)
		end
	end,
	opts = function()
		local wh = vim.api.nvim_win_get_height(0)
		local ph = vim.o.previewheight
		return {
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
						["ctrl-h"] = { require("fzf-lua.actions").git_stage, require("fzf-lua.actions").resume },
						["ctrl-l"] = { require("fzf-lua.actions").git_unstage, require("fzf-lua.actions").resume },
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
			keymap = {
				-- These override the default tables completely
				-- no need to set to `false` to disable a bind
				-- delete or modify is sufficient
				builtin = {
					-- neovim `:tmap` mappings for the fzf win
					["<F1>"] = "toggle-help",
					["<F2>"] = "toggle-fullscreen",
					-- Only valid with the 'builtin' previewer
					["<F3>"] = "toggle-preview-wrap",
					["<F4>"] = "toggle-preview",
					-- Rotate preview clockwise/counter-clockwise
					["<F5>"] = "toggle-preview-ccw",
					["<F6>"] = "toggle-preview-cw",
					["<S-down>"] = "preview-page-down",
					["<S-up>"] = "preview-page-up",
					["<S-left>"] = "preview-page-reset",
				},
				fzf = {
					-- fzf '--bind=' options
					["ctrl-z"] = "abort",
					["ctrl-u"] = "unix-line-discard",
					["ctrl-f"] = "half-page-down",
					["ctrl-b"] = "half-page-up",
					["ctrl-a"] = "beginning-of-line",
					["ctrl-e"] = "end-of-line",
					["alt-a"] = "toggle-all",
					-- Only valid with fzf previewers (bat/cat/git/etc)
					["f3"] = "toggle-preview-wrap",
					["f4"] = "toggle-preview",
					["shift-down"] = "preview-page-down",
					["shift-up"] = "preview-page-up",
				},
			},
		}
	end,
	config = function(_, opts)
		require("fzf-lua").setup(opts)
		-- Register fzf-lua as vim.ui.select
		require("fzf-lua").register_ui_select()

		-- :Files command
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

		-- :Buffers command
		vim.api.nvim_create_user_command("Buffers", function()
			require("fzf-lua").buffers()
		end, {
			force = true,
		})

		vim.keymap.set("n", "<Plug>(dotfiles-fzf-files)", "<cmd>Files<CR>", { silent = true })
		vim.keymap.set("n", "<Plug>(dotfiles-fzf-buffers)", "<cmd>Buffers<CR>", { silent = true })
	end,
}
