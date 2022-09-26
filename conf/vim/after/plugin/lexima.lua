local function make_markdown_bi_rule(char, escape)
	local esc_char = escape and [[\]] .. char or char
	return {
		{
			char = char,
			input_after = char,
			filetype = "markdown",
			except = [[^]] .. esc_char .. [[\{0,1\}\%#]],
		}, -- Create italic pair
		{
			char = char,
			at = [[\%#]] .. esc_char,
			leave = char,
			filetype = "markdown",
			except = esc_char .. [[\{1\}\%#]],
		}, -- Leave italic pair
		{
			char = char,
			at = esc_char .. esc_char .. [[.\+\%#]] .. esc_char,
			leave = 2,
			filetype = "markdown",
			except = esc_char .. [[\{1\}\%#]],
		}, -- Leave bold pair
		{
			char = "<BS>",
			at = esc_char .. [[\%#]] .. esc_char,
			delete = char,
			filetype = "markdown",
		},
	} -- Delete pair
end

-- XML-style closetag:
local function xml_closetag_rules()
	if vim.g.lexima_disable_closetag == 0 then
		local output = {}
		for _, ft in pairs({ "html", "xml", "javascript", "javascriptreact" }) do
			table.insert(output, { char = "<", input_after = ">", filetype = ft })
			table.insert(output, {
				char = ">",
				at = [[<\(\w\+\)\%#>]],
				leave = 1,
				input_after = [[</\1>]],
				with_submatch = 1,
				filetype = ft,
			})
		end
		return output
	end
	return nil
end

local function lua_endwise_rules()
	-- Lua endwise rules:
	if vim.g.lexima_enable_endwise_rules == 1 then
		-- local output = {}
		local function make_endwise_rule(at, ed, ft, syn)
			return {
				char = "<CR>",
				input = "<CR>",
				input_after = "<CR>" .. ed,
				at = at,
				except = [[\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1]] .. ed,
				filetype = ft,
				syntax = syn,
			}
		end
		return {
			make_endwise_rule([[^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {}),
			make_endwise_rule([[^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {}),
			make_endwise_rule([[^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {}),
		}
	end
	return nil
end

vim.g.dotfiles_lexima_rules = {
	-- Markdown rules:
	-- Links:
	{
		char = "]",
		at = [=[\[[^]]*\%#\]]=],
		except = [=[\[@[^]]*\%#\]]=],
		leave = "]",
		input = "(",
		input_after = ")",
		filetype = "markdown",
	},
	-- Blockquotes:
	{
		char = "<BS>",
		input = "<BS><BS>",
		at = [[^> \%#]],
		delete = 2,
		filetype = "markdown",
	},
	{
		char = "<CR>",
		at = "^> ",
		input = "<CR>> ",
		filetype = "markdown",
	},
	{
		char = ">",
		input = "> ",
		at = [[^\%#]],
		filetype = "markdown",
	},
	-- Tasks:
	{
		char = "[",
		input = "[ ]",
		at = [[^\s*[*-]\s*\%#]],
		filetype = "markdown",
	},
	{
		char = "<BS>",
		input = "<BS><BS><BS>",
		delete = 3,
		at = [[^\s*[*-]\s*\[.\]\%#]],
		filetype = "markdown",
	},
	-- Bold/Italic Pairs:
	make_markdown_bi_rule("*", true),
	make_markdown_bi_rule("_"),

	-- XML closing tags:
	xml_closetag_rules(),

	-- Insert Lua endwise rules:
	lua_endwise_rules(),
}
