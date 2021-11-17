" Turn off FZF trigger for easier QF navigation:
nmap <buffer> <C-P> <C-P>

" Open in a split:
lua << EOF
local status,_ = pcall(require, "qf_helper")
if status then
	local map = require("dotfiles.utils.map")
	map.nnoremap("<silent><buffer>", "<C-s>", "<cmd>lua require('qf_helper').open_split('split')<CR>")
	-- Open in a vertical split:
	map.nnoremap("<silent><buffer>", "<C-v>", "<cmd>lua require('qf_helper').open_split('vsplit')<CR>")
	-- Open without leaving quickfix:
	map.nnoremap("<silent><buffer>", "<C-y>", "<CR><C-W>p")
	map.nnoremap("<silent><buffer>", "<C-j>", "j<CR><C-W>p")
	map.nnoremap("<silent><buffer>", "<C-k>", "k<CR><C-W>p")
	map.nnoremap("<silent><buffer>", "{", "<cmd>lua require('qf_helper').navigate(-1, { by_file = true })<CR><C-w>p")
	map.nnoremap("<silent><buffer>", "}", "<cmd>lua require('qf_helper').navigate(1, { by_file = true })<CR><C-w>p")
end
EOF
