vim.cmd([[set visualbell t_vb=]]) -- Disable visual bell
vim.opt.autowrite = true --  Autosave files
vim.opt.hidden = true --  turn off buffer saving when switching
vim.opt.lazyredraw = true --  Don't redraw between macro runs (may make terminal flicker)

-- Override Default Split Creation Locations:
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Line Numbering:
vim.opt.number = true
vim.opt.relativenumber = true

-- Folds:
vim.opt.foldlevel = 99
vim.opt.foldmethod = "manual"

-- Avoid Highlighting Large Files:
vim.g.large_file = 20 * 1024 * 1024

-- Set Leader:
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Use split for search/replace preview:
vim.opt.inccommand = "split"

-- Height Of The Preview Window:
vim.opt.previewheight = 14

-- Completion:
vim.opt.completeopt = "menuone,noselect,noinsert,preview"
vim.opt.shortmess = vim.opt.shortmess + "c"
-- prevent a condition where vim lags due to searching include files.
vim.opt.complete = vim.opt.complete - "i"

-- <C-z> expands wildcards in command mode
vim.opt.wildcharm = vim.fn.char2nr("^Z")
-- Set path to current file direction and pwd:
vim.opt.path = ".,,"

-- Use better grep, if available:
if vim.fn.executable("rg") == 1 then
	vim.opt.grepprg = "rg --vimgrep --smart-case"
	vim.opt.grepformat = "%f:%l:%c:%m"
elseif vim.fn.executable("ag") == 1 then
	vim.opt.grepprg = "ag --vimgrep"
	vim.opt.grepformat = "%f:%l:%c:%m"
else
	vim.opt.grepprg = "grep -rn"
end

-- Searching:
vim.opt.wrapscan = true -- Start scan over at the top

-- Linewrap:
-- vim.opt.wrap = false
vim.opt.sidescroll = 5 -- Unused without set wrap, but prepared in case it is used
vim.opt.showbreak = "↳ " -- Show a line has wrapped

vim.opt.dictionary = "/usr/share/dict/words"

-- Default to hashtag-style comments, by default:
vim.opt.commentstring = "# %s"


-- Minimal Statusbar:
vim.opt.statusline=" %0.45f%m%h%w%r%= %y %l:%c "

-- Mouse And Clipboard:
vim.opt.mouse = "a" -- Mouse support
if vim.fn.has("clipboard") == 1 then
	if vim.fn.has("unnamedplus") == 1 then
		vim.opt.clipboard = "unnamedplus,unnamed"
	else
		vim.opt.clipboard = "unnamed"
	end
end

-- ==============================================================================
-- Disable Plugins:
-- ==============================================================================
vim.g.loaded_gzip = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_zipPlugin = 1
vim.g.loaded_2html_plugin = 1
vim.g.loaded_rrhelper = 1
vim.g.loaded_remote_plugins = 1
-- vim.g.loaded_netrw = 1
-- vim.g.loaded_netrwPlugin = 1
-- ==============================================================================
-- Autocommands:
-- ==============================================================================
vim.api.nvim_create_augroup("dotfiles-settings", { clear = true })
-- Line Number Colors in default:
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = "dotfiles-settings", pattern = "default", command = "hi LineNr ctermfg=7" }
)
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = "dotfiles-settings", pattern = "default", command = "hi LineNrAbove ctermfg=7" }
)
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{ group = "dotfiles-settings", pattern = "default", command = "hi LineNrBelow ctermfg=7" }
)
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{
		group = "dotfiles-settings",
		pattern = "default",
		command = "hi StatusLine ctermbg=8 ctermfg=7 cterm=NONE gui=NONE",
	}
)
vim.api.nvim_create_autocmd(
	"ColorScheme",
	{
		group = "dotfiles-settings",
		pattern = "default",
		command = "hi StatusLineNC ctermbg=8 ctermfg=240 cterm=NONE gui=NONE",
	}
)

-- Turn Off Line Numbering:
vim.api.nvim_create_autocmd(
	"TermOpen",
	{ group = "dotfiles-settings", command = "setlocal nonumber norelativenumber" }
)

-- Start QuickFix:
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	group = "dotfiles-settings",
	pattern = "[^l]*",
	callback = function()
		list_toggle("c", 1)
	end,
})
vim.api.nvim_create_autocmd("QuickFixCmdPost", {
	group = "dotfiles-settings",
	pattern = "l*",
	callback = function()
		list_toggle("l", 1)
	end,
})

-- Highlighted Yank:
vim.api.nvim_create_autocmd("TextYankPost", {
	group = "dotfiles-settings",
	command = [[silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=500}]],
})

-- Close Preview Window:
vim.api.nvim_create_autocmd("CompleteDone", {
	group = "dotfiles-settings",
	callback = function()
		if vim.fn.pumvisible() == 0 then
			vim.cmd("pclose")
		end
	end,
})

-- ==============================================================================
-- Basic Maps:
-- ==============================================================================
-- Clear Currently Highlighted Regexp:
vim.keymap.set("n", "<leader>cr", ':let<C-u>let @/=""<CR>', { silent = true, noremap = true })

-- Navigate Buffers:
vim.keymap.set("n", "]b", "<cmd>bnext<CR>", { silent = true, noremap = true })
vim.keymap.set("n", "[b", "<cmd>bprevious<CR>", { silent = true, noremap = true })

-- Source https://github.com/romainl/minivimrc/blob/master/vimrc
-- Minimal File Finding:
vim.keymap.set("n", "<localleader>f", ":find *", { noremap = true })
vim.keymap.set("n", "<localleader>s", ":sfind *", { noremap = true })
vim.keymap.set("n", "<localleader>v", ":vert sfind *", { noremap = true })
-- Minimal Buffer Jumping:
vim.keymap.set("n", "<leader>a", ":buffers<CR>:buffer<Space> ", { noremap = true })
vim.keymap.set("n", "<localleader>a", ":buffer *", { noremap = true })
vim.keymap.set("n", "<localleader>A", ":sbuffer *", { noremap = true })

-- Navigate Quickfix:
vim.keymap.set("n", "]q", "<cmd>cnext<CR>", { silent = true, noremap = true })
vim.keymap.set("n", "[q", "<cmd>cprevious<CR>", { silent = true, noremap = true })

-- Navigate Location List:
vim.keymap.set("n", "]d", "<cmd>lnext<CR>", { silent = true, noremap = true })
vim.keymap.set("n", "[d", "<cmd>lprev<CR>", { silent = true, noremap = true })

-- Toggle Quickfix:
vim.keymap.set("n", "<leader>q", function()
	list_toggle("c")
end, { silent = true, noremap = true })
vim.keymap.set("n", "<leader>d", function()
	list_toggle("l")
end, { silent = true, noremap = true })

-- Project Grep:
vim.keymap.set("n", "<leader>/", function()
	grep_or_qfgrep()
end, { silent = true, noremap = true })

-- Highlight a block and type "@" to run a macro on the block:
vim.keymap.set("x", "@", require("dotfiles.visualat"), { silent = true, noremap = true })

-- Calculator:
vim.keymap.set("i", "<C-A>", "<C-O>yiW<End>=<C-R>=<C-R>0<CR>", { silent = true, noremap = true })

-- Vertical split like in my Tmux config
vim.keymap.set("n", "<C-W>S", "<cmd>vsplit<cr>")

-- When text is wrapped, move by terminal rows, not lines, unless a count is provided
vim.keymap.set("n", "j", "(v:count == 0 ? 'gj' : 'j')", { silent = true, noremap = true, expr = true })
vim.keymap.set("n", "k", "(v:count == 0 ? 'gk' : 'k')", { silent = true, noremap = true, expr = true })

-- Textobjects:
-- Fold Maps:
vim.keymap.set("o", "iz", "<cmd>normal! [zj0v]zk$<cr>", { silent = true, noremap = true })
vim.keymap.set("x", "iz", "<cmd>normal! [zj0o]zk$<cr>", { silent = true, noremap = true })
vim.keymap.set("o", "az", "<cmd>normal! [zv]z$<cr>", { silent = true, noremap = true })
vim.keymap.set("x", "az", "<cmd>normal! [zo]z$<cr>", { silent = true, noremap = true })

-- Entire document:
vim.keymap.set("o", "ae", "<cmd>normal! gg0vG$<cr>", { silent = true, noremap = true })
vim.keymap.set("x", "ae", "<cmd>normal! gg0oG$<cr>", { silent = true, noremap = true })
-- ==============================================================================
-- Commands:
-- ==============================================================================
vim.api.nvim_create_user_command("Diagnostics", function()
	vim.cmd("silent lmake! %")
	if #vim.fn.getloclist(0) == 0 then
		vim.cmd("lopen")
	else
		vim.cmd("lclose")
	end
end, {
	force = true,
})
vim.api.nvim_create_user_command("Format", "silent normal! mxgggqG`x<CR>", {
	force = true,
})

-- Adjust Spacing:
vim.api.nvim_create_user_command("Spaces", function(args)
	local wv = vim.fn.winsaveview()
	vim.opt_local.expandtab = true
	vim.opt_local.listchars = vim.opt_local.listchars + "multispace:│" .. vim.fn["repeat"](" ", args.args)
	vim.cmd("silent execute '%!expand -it" .. args.args .. "'")
	vim.fn.winrestview(wv)
	vim.cmd("setlocal ts? sw? sts? et?")
end, {
	force = true,
	nargs = 1,
})
vim.api.nvim_create_user_command("Tabs", function(args)
	local wv = vim.fn.winsaveview()
	vim.opt_local.expandtab = false
	vim.cmd("silent execute '%!unexpand -t" .. args.args .. "'")
	vim.fn.winrestview(wv)
	vim.cmd("setlocal ts? sw? sts? et?")
end, {
	force = true,
	nargs = 1,
})
-- ==============================================================================
-- Functions:
-- ==============================================================================

-- Open or close quickfix or loclist
local function list_toggle(pfx, force_open)
	if not force_open then
		local status = vim.g["dotfiles_" .. pfx .. "open"] or 0
		if status ~= 0 then
			vim.g["dotfiles_" .. pfx .. "open"] = 0
			vim.cmd(pfx .. "close")
			return
		end
		if pfx == "l" and vim.fn.len(vim.fn.getloclist(0)) == 0 then
			vim.cmd([[echohl ErrorMsg
			echo 'Location List is Empty.'
			echohl NONE]])
			return
		end
	end
	vim.g["dotfiles_" .. pfx .. "open"] = 1
	vim.cmd(pfx .. "open")
end

-- Run grep! unless we're in quickfix results, then run cfilter
local function grep_or_qfgrep()
	if vim.opt.buftype:get() == "quickfix" then
		-- Load cfilter in quickfix view:
		vim.cmd([[packadd cfilter]])
		local input = vim.fn.input("QFGrep/")
		if #input > 0 then
			local prefix = vim.fn.getwininfo(vim.fn.win_getid())[1].loclist == 1 and "L" or "C"
			vim.cmd(prefix .. "filter /" .. input .. "/")
		end
	else
		local input = vim.fn.input("Grep/")
		if #input > 0 then
			vim.cmd('silent! grep! "' .. input .. '"')
		end
	end
end
-- ==============================================================================
-- Minpac For Essentials:
-- ==============================================================================
function! s:packpath() abort
	return ($XDG_DATA_HOME ? $XDG_DATA_HOME : $HOME.'/.local/share') . '/minpac/minimal'
endfunction

if !get(g:, 'skip_minimal_minpac', !has('patch-7.4.1384'))
	function! PackInit() abort
		-- Download Minpac:
		let l:minpac_dir = s:packpath()
		if empty(glob(l:minpac_dir.'/pack/minpac/opt/minpac'))
			if executable('git')
				silent execute '!git clone --depth 1 https://github.com/k-takata/minpac "'.l:minpac_dir.'/pack/minpac/opt/minpac"'
			endif
		endif

		-- Load Minpac:
		packadd minpac

		if exists('g:loaded_minpac')
			call minpac#init({'dir': l:minpac_dir})
			call minpac#add('tpope/vim-sensible', {'type': 'opt'}) " Agreeable vim settings:
			call minpac#add('xero/securemodelines', {'type': 'opt'}) " Secure modelines
			call minpac#add('tpope/vim-repeat') " dot repeat for plugins
			call minpac#add('tpope/vim-commentary') " gc to toggle comments
			call minpac#add('oncomouse/vim-surround') " ys to add, cs to change, ds to delete. f, F for function, t, T for tag
			call minpac#add('oncomouse/vim-lion') " gl and gL to align
			call minpac#add('vim-scripts/ReplaceWithRegister') " gr{motion} or grr or gr in visual to replace with register
			call minpac#add('k-takata/minpac', {'type': 'opt'})
			call minpac#add('noahfrederick/vim-neovim-defaults', {'type': 'opt'}) " Set Vim with the same default as NeoVim
		else
			echoerr "Could not load minpac. Perhaps your Internet is not working or you don't have git?"
		endif
	endfunction

	exe 'set packpath+='.s:packpath()
	command! PackUpdate call PackInit() | call minpac#update()
	command! PackClean call PackInit() | call minpac#clean()
	command! PackStatus call PackInit() | call minpac#status()
end

-- ==============================================================================
-- Pack Settings:
-- ==============================================================================
if has('patch-7.4.1384') " packadd available
	if !has('patch-8.1.1365') && !has('nvim-0.4') " Securemodelines is needed:
		set nomodeline
		vim.g.secure_modelines_verbose = 0
		vim.g.secure_modelines_modelines = 15
		vim.g.secure_modelines_allowed_items = [
			\ 'textwidth',	 'tw',
			\ 'softtabstop', 'sts',
			\ 'tabstop',     'ts',
			\ 'shiftwidth',	 'sw',
			\ 'expandtab',	 'et',	 'noexpandtab', 'noet',
			\ 'filetype',    'ft',
			\ 'foldmethod',	 'fdm',
			\ 'foldlevel',	 'fdl',
			\ 'readonly',    'ro',	 'noreadonly', 'noro',
			\ 'rightleft',	 'rl',	 'norightleft', 'norl',
			\ 'cindent',     'cin',	 'nocindent', 'nocin',
			\ 'smartindent', 'si',	 'nosmartindent', 'nosi',
			\ 'autoindent',	 'ai',	 'noautoindent', 'noai',
			\ 'spell',       'nospell',
			\ 'spelllang',
			\ 'wrap',        'nowrap',
			\ 'syntax'
			\ ]
		packadd! securemodelines
	endif
	if !has('nvim') " Not using Neovim:
		packadd! vim-sensible
		packadd! vim-neovim-defaults
	endif
	if !exists('g:skip_minimal_minpac')
		colorscheme default
	endif
endif

-- ==============================================================================
-- FZF:
-- ==============================================================================
if isdirectory('/usr/local/opt/fzf') " Homebrew
	set runtimepath+=/usr/local/opt/fzf
	vim.g.has_fzf = 1
elseif isdirectory('/usr/share/vim/vimfiles') " Arch, btw
	set runtimepath+=/usr/share/vim/vimfiles
	vim.g.has_fzf = 1
elseif isdirectory('~/.fzf') " Local install
	set runtimepath+=~/.fzf
	vim.g.has_fzf = 1
end
vim.g.fzf_layout = { 'window': { 'width': 1, 'height': 0.4, 'yoffset': 1, 'border': 'top' } }
vim.g.fzf_action = {
	\ 'ctrl-s': 'split',
	\ 'ctrl-v': 'vsplit',
	\ 'ctrl-t': 'tabnew',
	\ 'ctrl-e': 'edit',
	\ }
vim.g.fzf_nvim_statusline = 0
if !exists('g:skip_minimal_minpac')
	vim.g.fzf_colors =
		\ { 'fg':      ['fg', 'Normal'],
		\   'bg':      ['bg', 'Normal'],
		\   'hl':      ['fg', 'Comment'],
		\   'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
		\   'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
		\   'hl+':     ['fg', 'Statement'],
		\   'info':    ['fg', 'PreProc'],
		\   'border':  ['fg', 'Ignore'],
		\   'prompt':  ['fg', 'Conditional'],
		\   'pointer': ['fg', 'Exception'],
		\   'marker':  ['fg', 'Keyword'],
		\   'spinner': ['fg', 'Label'],
		\   'header':  ['fg', 'Comment'] }
endif
if get(g:, 'has_fzf', 0)
	nmap <silent> <C-P> <cmd>FZF --reverse --info=inline<cr>
endif
