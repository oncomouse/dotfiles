-- luacheck: globals vim
local function wordcount()
	local ft = vim.bo.filetype
	if ft == 'markdown' or ft == 'vimwiki' or ft == 'txt' then
		return vim.fn.wordcount().words .. ' words'
	else
		return ''
	end
end
require('lualine').setup{
	options = {
		section_separators = '',
		component_separators = '',
		theme = 'wal',
	},
	sections = {
		lualine_a = {
			'mode'
		},
		lualine_b = {},
		lualine_c = {
			{
				'filename',
				full_path = false,
				shorten = false,
			}
		},
		lualine_x = {
			{
				'filetype',
				format = function(x) return '[' .. x .. ']' end,
			},{
				wordcount,
				color = {
					gui='none',
				},
			},
		},
		lualine_y = {},
		lualine_z = {
			{
				'location',
				color = {
					gui='none',
				},
				padding = 0,
				left_padding = 0,
				right_padding = 1,
			}, {
				'diagnostics',
				sources = { 'ale' },
				symbols = {error = ' ', warn = ' ', info = ' '},
			},
		},
	},
}
