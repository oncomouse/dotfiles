require('lualine').setup{
  options = {
	section_separators = '',
	component_separators = '',
	theme = 'wal',
  },
  sections = {
	lualine_a = {'mode'},
	lualine_b = {'branch'},
	lualine_c = {'filename'},
	lualine_x = {'encoding', 'filetype'}, -- 'fileformat',
	lualine_y = {'progress'},
	lualine_z = {'location', {
		'diagnostics',
		sources = { 'ale' },
		symbols = {error = ' ', warn = ' ', info = ' '}
	},},
  },
}
