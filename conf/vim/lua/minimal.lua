-- Make sure dotfiles are added to RTP, if not already added:
if vim.fn.isdirectory('~/dotfiles/conf/vim/') and string.find(vim.o.runtimepath, vim.fn.expand('~/dotfiles/conf/vim')) ~= nil then
	vim.o.runtimepath = vim.o.runtimepath .. ',~/dotfiles/conf/vim'
end
-- ==============================================================================
-- Basic Settings:
-- ==============================================================================
vim.o.visualbell=true
vim.o.t_b = '' -- Disable visual bell
vim.o.autowrite=true -- Autosave files
vim.o.hidden=true -- turn off buffer saving when switching
vim.o.lazyredraw=true -- Don't redraw between macro runs (may make terminal flicker)

-- Override Default Split Creation Locations:
vim.o.splitbelow=true
vim.o.splitright=true

-- Line Numbering:
vim.wo.number=true
vim.wo.relativenumber=true

-- Folds:
vim.bo.foldlevel=99
vim.bo.foldmethod="indent"

-- This avoids highlighting big files:
vim.g.large_file = 20*1024*1024

-- Set Leader:
vim.g.mapleader = " "
vim.g.maplocalleader = ","

vim.api.nvim_command("augroup minimal-vimrc")
vim.api.nvim_command("autocmd!")
vim.api.nvim_command("augroup END")

-- Preview Substitution Operations:
vim.o.inccommand="split"

-- Turn Off Line Numbering:
vim.api.nvim_command("autocmd minimal-vimrc TermOpen * setlocal nonumber norelativenumber")

-- Height Of The Preview Window:
vim.o.previewheight=14

-- Completion:
vim.o.completeopt="menuone,noselect,noinsert,preview"
-- Shut off completion messages
vim.o.shortmess=vim.o.shortmess .. "c"
-- prevent a condition where vim lags due to searching include files.
-- vim.o.complete-=i

-- <C-z> expands wildcards in command mode
vim.o.wildcharm=26
-- vim.api.nvim_command("set wildcharm=<C-z>")
-- Set path to current file direction and pwd:
vim.o.path=".,,"

-- Mouse And Clipboard:
vim.o.mouse="a" -- Mouse support
if vim.fn.has('clipboard') then
	if vim.fn.has('unnamedplus') then
		vim.o.clipboard="unnamedplus,unnamed"
	else
		vim.o.clipboard="unnamed"
	end
end

-- ==============================================================================
-- Basic Maps:
-- ==============================================================================
-- Select Whole File:
vim.api.nvim_set_keymap("n", "<leader>vf", "ggVG", { noremap=true })
-- Clear Currently Highlighted Regexp:
vim.api.nvim_set_keymap("n", "<leader>cr", ":let<C-u>let @/=\"\"<CR>", { silent=true, noremap=true })
-- Navigate Quickfix:
vim.api.nvim_set_keymap("n", "]q", "<cmd>cnext<CR>", { silent=true, noremap=true })
vim.api.nvim_set_keymap("n", "[q", "<cmd>cprevious<CR>", { silent=true, noremap=true })
-- Navigate Buffers:
vim.api.nvim_set_keymap("n", "]b", "<cmd>bnext<CR>", { silent=true, noremap=true })
vim.api.nvim_set_keymap("n", "[b", "<cmd>bprevious<CR>", { silent=true, noremap=true })

-- Source https://github.com/romainl/minivimrc/blob/master/vimrc
-- Minimal File Finding:
vim.api.nvim_set_keymap("n", "<localleader>f", ":find *", { noremap=true })
vim.api.nvim_set_keymap("n", "<localleader>s", ":sfind *", { noremap=true })
vim.api.nvim_set_keymap("n", "<localleader>v", ":vert sfind *", { noremap=true })
-- Minimal Buffer Jumping:
vim.api.nvim_set_keymap("n", "<localleader>b", ":buffer *", { noremap=true })
vim.api.nvim_set_keymap("n", "<localleader>B", ":sbuffer *", { noremap=true })
-- Completion Mappings:
-- Search previous words:
vim.api.nvim_buf_set_keymap(0, "i", "<localleader>,", "<C-x><C-n><C-r>=pumvisible() ? \"\\<lt>Down>\\<lt>C-p>\\<lt>Down>\\<lt>C-p>\" : \"\"<CR>", { silent=true, noremap=true })
-- Search file names:
vim.api.nvim_buf_set_keymap(0, "i", "<localleader>:", "<C-x><C-f><C-r>=pumvisible() ? \"\\<lt>Down>\\<lt>C-p>\\<lt>Down>\\<lt>C-p>\" : \"\"<CR>", { silent=true, noremap=true })
-- Search previous lines:
vim.api.nvim_buf_set_keymap(0, "i", "<localleader>=", "<C-x><C-l><C-r>=pumvisible() ? \"\\<lt>Down>\\<lt>C-p>\\<lt>Down>\\<lt>C-p>\" : \"\"<CR>", { silent=true, noremap=true })
-- commands for adjusting indentation rules manually
vim.api.nvim_command("command! -nargs=1 Spaces let b:wv = winsaveview() | execute \"setlocal expandtab\"   | silent execute \"%!expand -it \"	. <args> . \"\"  | call winrestview(b:wv) | setlocal ts? sw? sts? et?")
vim.api.nvim_command("command! -nargs=1 Tabs	 let b:wv = winsaveview() | execute \"setlocal noexpandtab\" | silent execute \"%!unexpand -t \" . <args> . \"\" | call winrestview(b:wv) | setlocal ts? sw? sts? et?")

-- <nop> those bindings for writing files:
vim.api.nvim_command("autocmd minimal-vimrc FileType markdown,text,mail silent! iunmap <buffer> <localleader>,")
vim.api.nvim_command("autocmd minimal-vimrc FileType markdown,text,mail silent! iunmap <buffer> <localleader>:")
vim.api.nvim_command("autocmd minimal-vimrc FileType markdown,text,mail silent! iunmap <buffer> <localleader>=")

-- Better Matching:
vim.api.nvim_set_keymap("n", "[I", "[I:ijump<Space><Space><Space><C-r><C-w><S-Left><Left><Left>", { noremap=true })
vim.api.nvim_set_keymap("n", "]I", "]I:ijump<Space><Space><Space><C-r><C-w><S-Left><Left><Left>", { noremap=true })
--
-- Line Number Colors:
vim.api.nvim_command("autocmd minimal-vimrc ColorScheme default hi LineNr ctermfg=7")
vim.api.nvim_command("autocmd minimal-vimrc ColorScheme default hi LineNrAbove ctermfg=7")
vim.api.nvim_command("autocmd minimal-vimrc ColorScheme default hi LineNrBelow ctermfg=7")
vim.api.nvim_command("autocmd minimal-vimrc ColorScheme default hi StatusLine ctermbg=8 ctermfg=7 cterm=bold")
vim.api.nvim_command("autocmd minimal-vimrc ColorScheme default hi StatusLineNC ctermbg=8 ctermfg=7 cterm=NONE")

-- Minimal Statusbar:
-- source: https://github.com/darioisthebest/dotfiles/blob/master/nvim/init.vim#L126
vim.o.statusline="%0.45f%m%r%h%w %=LN %l/%L C %c%V %P %y"
vim.api.nvim_command("colorscheme default")
