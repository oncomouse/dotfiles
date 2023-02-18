-- Sourced from: https://github.com/mars90226/dotvim/blob/master/lua/vimrc/plugins/nvim_treesitter.lua
local nvim_treesitter = {
	installed_parser = {
		"bash",
		"bibtex",
		"c",
		"cmake",
		"comment",
		"cpp",
		"css",
		"diff",
		"dockerfile",
		"fennel",
		"fish",
		"go",
		"graphql",
		"html",
		"http",
		"java",
		"javascript",
		"jsdoc",
		"json",
		"jsonc",
		"latex",
		"lua",
		"make",
		"ninja",
		"nix",
		"norg",
		"org",
		"perl",
		"php",
		"python",
		"r",
		"rasi",
		"regex",
		"ruby",
		"rust",
		"scss",
		"svelte",
		"tsx",
		"typescript",
		"vim",
		"vue",
		"xml",
		"yaml",
		"zig",
	},
	parser_configs = {
		xml = {
			install_info = {
				url = "https://github.com/Trivernis/tree-sitter-xml",
				files = { "src/parser.c" },
				generate_requires_npm = true,
				branch = "main",
			},
			filetype = "xml",
		},

		-- TODO: Use repo in https://github.com/serenadeai/tree-sitter-scss/pull/19
		scss = {
			install_info = {
				url = "https://github.com/goncharov/tree-sitter-scss",
				files = { "src/parser.c", "src/scanner.c" },
				branch = "placeholders",
				revision = "30c9dc19d3292fa8d1134373f0f0abd8547076e8",
			},
			maintainers = { "@goncharov" },
		},
	},
}
local utils = {}
utils.get_buffer_variable = function(buf, var)
	local status, result = pcall(vim.api.nvim_buf_get_var, buf, var)
	if status then
		return result
	end
	return nil
end

nvim_treesitter.line_threshold = {
	base = {
		cpp = 30000,
		javascript = 30000,
		perl = 10000,
	},
	extension = {
		cpp = 10000,
		javascript = 3000,
		perl = 3000,
	},
} -- Disable check for highlight, highlight usage, highlight context module

local force_disable_var = "nvim_treesitter_force_disable"
local get_force_disable = function(bufnr)
	return utils.get_buffer_variable(bufnr, force_disable_var) or false
end

local buffer_toggle_force_disable = function(bufnr)
	local force_disable = not (utils.get_buffer_variable(bufnr, force_disable_var) or false)
	vim.api.nvim_buf_set_var(bufnr, force_disable_var, force_disable)
end

local disable_check = function(type, lang, bufnr)
	if get_force_disable(bufnr) then
		return true
	end
	if type == nil then
		type = "base"
	end

	local line_count = vim.api.nvim_buf_line_count(bufnr or 0)
	local line_threshold_map = vim.F.if_nil(nvim_treesitter.line_threshold[type], {})
	local line_threshold = line_threshold_map[lang]

	if line_threshold ~= nil and line_count > line_threshold then
		return true
	else
		return false
	end
end

-- Disable check for highlight module
local base_disable_check = function(lang, bufnr)
	return disable_check("base", lang, bufnr)
end

local current_buffer_base_highlight_disable_check = function()
	local ft = vim.bo.ft
	local bufnr = vim.fn.bufnr()
	return base_disable_check(ft, bufnr)
end

nvim_treesitter.setup_config = function(opts)
	local ts_foldexpr_augroup_id = vim.api.nvim_create_augroup("nvim_treesitter_foldexpr")

	vim.api.nvim_create_autocmd("FileType", {
		pattern = vim.fn.join(opts.ensure_installed, ","),
		group = ts_foldexpr_augroup_id,
		callback = function()
			vim.keymap.set("n", "<F6>", function()
				buffer_toggle_force_disable(vim.api.nvim_get_current_buf())
				vim.cmd([[TSBufToggle highlight]])
			end, { buffer = true })
			vim.opt_local.foldexpr = "nvim_treesitter#foldexpr()"
			vim.opt_local.foldmethod = "expr"
		end,
		desc = "Set fold method for treesitter",
	})

	require("nvim-treesitter.configs").setup(opts)
end

nvim_treesitter.setup_parser_config = function()
	local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
	for f, c in pairs(nvim_treesitter.parser_configs) do
		parser_configs[f] = c
	end
end

nvim_treesitter.setup_extensions = function()
	local ts_highlight_check_augroup_id = vim.api.nvim_create_augroup("nvim_treesitter_highlight_check", {})
	-- FIXME: Currently this doesn't disable the modules as they are enabled in
	-- nvim-treesitter attach callback, which is executed after this autocmd.
	-- But this can fix the error that highlight is disabled on startup and also
	-- enables modules after startup.
	vim.api.nvim_create_autocmd({ "BufEnter" }, {
		group = ts_highlight_check_augroup_id,
		pattern = "*",
		callback = function()
			if current_buffer_base_highlight_disable_check() then
				vim.schedule(function()
					vim.cmd([[NoMatchParen]])
				end)
			else
				vim.schedule(function()
					vim.cmd([[DoMatchParen]])
				end)
			end
		end,
	})
end

nvim_treesitter.setup_performance_trick = function()
	local configs_commands = require("nvim-treesitter.configs").commands

	-- TODO: Check if these actually help performance, initial test reveals that these may reduce highlighter time, but increase "[string]:0" time which is probably the time spent on autocmd & syntax enable/disable.
	-- TODO: These config help reduce memory usage, see if there's other way to fix high memory usage.
	-- TODO: Change to tab based toggling
	local augroup_id = vim.api.nvim_create_augroup("nvim_treesitter_settings", {})

	local global_idle_disabled_modules = vim.tbl_filter(function(module)
		return module ~= nil
	end, {
		"highlight",
		"context_commentstring",
		"matchup",
		"navigation",
		"smart_rename",
	})
	local tab_idle_disabled_modules = global_idle_disabled_modules

	local global_trick_delay_enable = false
	local global_trick_delay = 10 * 1000 -- 10 seconds
	vim.api.nvim_create_autocmd({ "FocusGained" }, {
		group = augroup_id,
		pattern = "*",
		callback = function()
			if global_trick_delay_enable then
				global_trick_delay_enable = false
			else
				for _, module in ipairs(global_idle_disabled_modules) do
					configs_commands.TSEnable.run(module)
				end
			end
		end,
	})
	-- NOTE: We want to disable highlight if FocusLost is caused by following reasons:
	-- 1. neovim goes to background
	-- 2. tmux switch window, client
	-- 3. Terminal emulator switch tab
	-- We don't want to disable highlight if FocusLost is caused by following reasons:
	-- 1. tmux switch pane
	-- 2. Terminal emulator switch pane
	-- 3. OS switch application
	-- In other words, we want treesitter highlight if the buffer is actually displayed on the screen.
	vim.api.nvim_create_autocmd({ "FocusLost" }, {
		group = augroup_id,
		pattern = "*",
		callback = function()
			global_trick_delay_enable = true

			vim.defer_fn(function()
				if global_trick_delay_enable then
					for _, module in ipairs(global_idle_disabled_modules) do
						configs_commands.TSDisable.run(module)
					end

					global_trick_delay_enable = false
				end
			end, global_trick_delay)
		end,
	})

	local tab_trick_enable = false
	local tab_trick_debounce = 200
	-- FIXME: Open buffer in other tab doesn't have highlight
	vim.api.nvim_create_autocmd({ "TabEnter" }, {
		group = augroup_id,
		pattern = "*",
		callback = function()
			tab_trick_enable = true

			vim.defer_fn(function()
				if tab_trick_enable then
					local winids = vim.api.nvim_tabpage_list_wins(0)

					for _, module in ipairs(tab_idle_disabled_modules) do
						for _, winid in ipairs(winids) do
							configs_commands.TSBufEnable.run(module, vim.api.nvim_win_get_buf(winid))
						end
					end

					tab_trick_enable = false
				end
			end, tab_trick_debounce)
		end,
	})
	vim.api.nvim_create_autocmd({ "TabLeave" }, {
		group = augroup_id,
		pattern = "*",
		callback = function()
			for _, winid in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
				for _, module in ipairs(tab_idle_disabled_modules) do
					configs_commands.TSBufDisable.run(module, vim.api.nvim_win_get_buf(winid))
				end
			end
		end,
	})
end
-- End sourced material

return {
	{
		"oncomouse/vim-markdown",
		ft = "markdown",
		init = function()
			vim.g.vim_markdown_frontmatter = 1 -- Format YAML
			vim.g.vim_markdown_strikethrough = 1 -- Don't format strikethrough
			vim.g.vim_markdown_conceal = 0 -- Don't conceal
			vim.g.vim_markdown_conceal_code_blocks = 0 -- Don't conceal code blocks
			vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
			vim.g.vim_markdown_auto_insert_bullets = 0 -- autoList handles bullet insertion now
			vim.g.vim_markdown_new_list_item_indent = 0 -- autoList handles spacing for lists
		end,
	}, -- Markdown Syntax

	{
		"nvim-treesitter/nvim-treesitter",
		build = function()
			vim.cmd([[TSUpdate]])
		end,
		opts = {
			ensure_installed = nvim_treesitter.installed_parser,
			highlight = { enable = true },
			context_commentstring = { enable = true },
			autotag = { enable = true },
			matchup = { enable = true },
			textobjects = {
				select = {
					enable = true,
					-- Automatically jump forward to textobj, similar to targets.vim
					lookahead = true,
					keymaps = {
						-- You can use the capture groups defined in textobjects.scm
						["af"] = "@function.outer",
						["if"] = "@function.inner",
						["ac"] = "@class.outer",
						-- you can optionally set descriptions to the mappings (used in the desc parameter of nvim_buf_set_keymap
						["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
					},
				},
				move = {
					enable = true,
					set_jumps = true, -- whether to set jumps in the jumplist
					goto_next_start = {
						["]m"] = "@function.outer",
					},
					goto_next_end = {
						["]M"] = "@function.outer",
					},
					goto_previous_start = {
						["[m"] = "@function.outer",
					},
					goto_previous_end = {
						["[M"] = "@function.outer",
					},
				},
			},
		},
		config = function(_, opts)
			nvim_treesitter.setup_config(opts)
			nvim_treesitter.setup_parser_config()
			nvim_treesitter.setup_extensions()
			nvim_treesitter.setup_performance_trick()
		end,
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			"windwp/nvim-ts-autotag",
			{
				"andymass/vim-matchup",
				event = "FileType",
				init = function()
					vim.g.matchup_matchparen_offscreen = {
						method = "popup",
					}
				end,
			},
			{
				"JoosepAlviste/nvim-ts-context-commentstring", -- Contextual commentstring
				config = function()
					require("mini.comment").setup({
						hooks = {
							pre = function()
								require("ts_context_commentstring.internal").update_commentstring()
							end,
						},
					})
				end,
			},
		},
	},
}