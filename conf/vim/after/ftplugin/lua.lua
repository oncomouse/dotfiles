-- Create custom surrouding for Lua's block string `[[...]]`. Use this inside
--
-- autocommand or 'after/ftplugin/lua.lua' file.
vim.b.minisurround_config = {
	custom_surroundings = {
		s = {
			input = { find = "%[%[.-%]%]", extract = "^(..).*(..)$" },
			output = { left = "[[", right = "]]" },
		},
	},
}
