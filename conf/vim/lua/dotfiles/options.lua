-- Basic Settings {{{
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

-- Use split for search/replace preview:
vim.opt.inccommand = "split"

-- Height Of The Preview Window:
vim.opt.previewheight = 14

-- Cmdheight=0 options:
vim.opt.cmdheight = 0
if vim.fn.has("nvim-0.9") == 1 then
	vim.opt.showcmdloc = "statusline"
	-- Add some other Neovim 0.9 things here:
	vim.opt.shortmess:append("C")
	vim.opt.splitkeep = "screen"
end
vim.opt.showmode = false

-- Completion:
vim.opt.completeopt = "menuone,noselect,noinsert,preview"
vim.opt.shortmess:append("Wc")
-- prevent a condition where vim lags due to searching include files.
vim.opt.complete:remove("i")

-- <C-z> expands wildcards in command mode
vim.cmd([[set wildcharm=<C-z>]])
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

-- Minimal statusline (used if notermguicolors is set):
vim.opt.statusline = " %0.45f%m%h%w%r%= %y %l:%c "

-- Searching:
vim.opt.wrapscan = true -- Start scan over at the top

-- Linewrap:
vim.opt.sidescroll = 5 -- Unused without set wrap, but prepared in case it is used
vim.opt.showbreak = "↳ " -- Show a line has wrapped

-- Mouse And Clipboard:
vim.opt.mouse = "a" -- Mouse support
if vim.fn.has("clipboard") == 1 then
	if vim.fn.has("unnamedplus") == 1 then
		vim.opt.clipboard = "unnamedplus,unnamed"
	else
		vim.opt.clipboard = "unnamed"
	end
end

vim.opt.dictionary = "/usr/share/dict/words"

-- Default to hashtag-style comments, by default:
vim.opt.commentstring = "# %s"

-- Options taken from mini.basics, which does too many things I don't want:
vim.opt.virtualedit = "block"
vim.opt.formatoptions = "qjl1"

-- Set Spellfile Location:
vim.opt.spellfile = "~/dotfiles/conf/vim/spell/en.utf-8.add"
-- }}}
-- Mac NeoVim Settings {{{
if vim.fn.has("mac") == 1 and vim.fn.has("nvim") == 1 then
	vim.g.python_host_prog = "/usr/bin/python2.7"
	vim.g.python3_host_prog = "/usr/local/bin/python3"
	vim.g.ruby_host_prog = vim.fn.expand("~/.asdf/shims/neovim-ruby-host")
	vim.g.node_host_prog = "/usr/local/lib/node_modules/neovim/bin/cli.js"
	--- This is macOS only, I believe, but it fixes slow start-up for clipboard:
	vim.g.clipboard = {
		copy = { ["+"] = "pbcopy", ["*"] = "pbcopy" },
		paste = { ["+"] = "pbpaste", ["*"] = "pbpaste" },
		name = "pbcopy",
		cache_enabled = 0,
	}
end
-- }}}
-- Tabs {{{
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = false
-- }}}
-- Signs {{{
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
	local hl = "DiagnosticSign" .. type
	vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end
-- }}}
-- Writing {{{
vim.g.bibfiles = "~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/library.bib"
-- }}}
-- Plugins {{{
require("rocks") -- Add luarocks to the path
-- todo {{{
require("todo").setup({
	maps = {
		jumps = {
			next = "",
			prev = "",
		},
	},
})
-- }}}
-- }}}
-- Theme {{{
-- Fancy color for macs and X11 sessions:
if require("dotfiles.utils.use_termguicolors")() then
	vim.cmd([[let &t_8f='<Esc>[38;2;%lu;%lu;%lum']])
	vim.cmd([[let &t_8b='<Esc>[48;2;%lu;%lu;%lum']])
	vim.opt.termguicolors = true

	vim.api.nvim_create_autocmd("ColorScheme", {
		pattern = "catppuccin*",
		callback = function()
			local colors = require("catppuccin.palettes").get_palette()
			require("catppuccin.lib.highlighter").syntax({
				gitCommitOverflow = { fg = colors.red },
				gitCommitSummary = { fg = colors.green },
			})
		end,
	})
	if not pcall(vim.cmd, [[colorscheme catppuccin-mocha]]) then
		vim.cmd([[colorscheme default]])
	end
else
	vim.cmd([[colorscheme default]])
end
-- }}}
-- LSP: {{{
vim.diagnostic.config({
	underline = true,
	virtual_text = true,
	signs = false,
	severity_sort = true,
})

-- Set to true for debug logging in LSP:
vim.g.dotfiles_lsp_debug = false
-- }}}
-- Filetypes {{{
vim.filetype.add({
	extension = {
		rasi = "rasi",
	},
	filename = {},
	pattern = {},
})
-- }}}
-- # vim:foldmethod=marker:foldlevel=0
