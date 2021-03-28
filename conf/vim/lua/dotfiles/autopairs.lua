-- luacheck: globals vim
local remap = vim.api.nvim_set_keymap
local npairs = require('nvim-autopairs')

_G.DotUtils = {}

_G.DotUtils.completion_confirm = function()
	return npairs.check_break_line_char()
end

-- remap('i' , '<CR>','v:lua.DotUtils.completion_confirm()', {expr = true , noremap = true})

npairs.setup()
