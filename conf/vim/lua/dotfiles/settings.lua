if vim.g.dotfiles_loaded_pack == 1 then
	dotfiles = _G.dotfiles or {}
	vim.cmd[[ packadd vim-sensible ]]
	vim.cmd[[ packadd securemodelines ]]
	vim.cmd[[ packadd vim-grep ]]
	vim.cmd[[ packadd pastefix.vim ]]
	vim.cmd[[ packadd vim-commentary ]]
	vim.cmd[[ packadd vim-pasta ]]
	vim.cmd[[ packadd vim-repeat ]]
	vim.cmd[[ packadd vim-surround ]]
	vim.cmd[[ packadd targets.vim ]]
	vim.cmd[[ packadd nvim-ts-autotag ]]
	vim.cmd[[ packadd vim-fish ]]
	vim.cmd[[ packadd tabular ]]
	vim.cmd[[ packadd vim-rooter ]]
	vim.g.rooter_patterns = {
		"Rakefile",
		 "package.json",
		 ".git/",
		 "Gemfile",
		 "pyproject.toml",
		 "setup.py",
	}
	-- Set path expansion to pwd only, especially with vim-rooter running:
	vim.opt.path=",,"
	vim.cmd[[ packadd FastFold ]]
	vim.g.fastfold_savehook = 1
	vim.g.fastfold_fold_command_suffixes =	{"x","X","a","A","o","O","c","C", "r", "R", "m", "M"}
	vim.g.fastfold_fold_movement_commands = {"]z", "[z", "zj", "zk"}
	vim.g.fastfold_minlines = 0
	vim.cmd[[ packadd lexima.vim ]]
	local function make_rule(at, e, filetype, syntax)
		return {
			char = "<CR>",
			input = "<CR>",
			input_after = "<CR>" .. e,
			at = at,
			except = "\\C\\v^(\\s*)\\S.*%#\\n%(%(\\s*|\\1\\s.+)\\n)*\\1" .. e,
			filetype = filetype,
			syntax = syntax,
		}
	end
	function dotfiles.extend_endwise_for_lua()
		-- Lua endwise rules:
		vim.fn["lexima#add_rule"](make_rule("^\\s*if\\>.*then\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {}))
		vim.fn["lexima#add_rule"](make_rule("^\\s*\\%(for\\|while\\)\\>.*do\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {}))
		vim.fn["lexima#add_rule"](make_rule("^\\s*\\%(local\\)\\=.*function\\>\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {}))
	end
	vim.cmd[[autocmd! dotfiles-settings FileType lua lua dotfiles.extend_endwise_for_lua()]]
	vim.api.nvim_set_keymap("i", "<C-l>", "<C-r>=lexima#insmode#leave_till_eol(\"\")<CR>", { noremap = true })
	vim.cmd[[ packadd nvim-colorizer.lua ]]
	if vim.fn.exists("+termguicolors") == 1 then
		require('colorizer').setup{
			'*',
			markdown = {
				names = false
			},
			text={
				names = false
			},
			["gina-commit"] = {
				names = false
			},
		}
	end
	vim.cmd[[ packadd gina.vim ]]
	vim.fn["gina#custom#command#option"]("status", "--opener", vim.o.previewheight .. "split")
	vim.fn["gina#custom#command#option"]("commit", "--opener", vim.o.previewheight .. "split")
	vim.fn["gina#custom#command#option"]("diff", "--opener", vim.o.previewheight .. "split")
	vim.fn["gina#custom#command#option"]("status", "--group", "short")
	vim.fn["gina#custom#command#option"]("commit", "--group", "short")
	vim.fn["gina#custom#command#option"]("diff", "--group", "short")
	vim.cmd[[ packadd fzf.vim ]]
	vim.cmd[[command! -bang -nargs=? -complete=dir Files call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--reverse', '--info=inline']}), <bang>0)]]
	vim.g.fzf_layout = { window = { width = 1, height = 0.4, yoffset = 1, border = 'top' } }
	vim.g.fzf_action = {
		["ctrl-s"] = 'split',
		["ctrl-v"] = 'vsplit',
		["ctrl-t"] = 'tabnew',
		["ctrl-e"] = 'edit',
	}
	vim.g.fzf_nvim_statusline = 0 -- disable statusline overwriting
	vim.cmd[[ packadd nvim-treesitter ]]
	require('nvim-treesitter.configs').setup{
		ensure_installed = "maintained",
		highlight = {
			enable = true,
		},
		indent = {
			enable = false,
		},
		autotag = {
			enable = true,
		}
	}
	vim.cmd[[ packadd vim-markdown ]]
	vim.g.vim_markdown_frontmatter = 1 -- Format YAML
	vim.g.vim_markdown_strikethrough = 0 -- Don"t format strikethrough
	vim.g.vim_markdown_conceal = 0 -- Don"t conceal
	vim.g.vim_markdown_conceal_code_blocks = 0 -- Don"t conceal code blocks
	vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
	vim.cmd[[ packadd nvim-lspconfig ]]
	require('dotfiles.nvim_lsp')
end
