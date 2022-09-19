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

local function neorg_pair(char, esc_char)
	esc_char = esc_char and esc_char or [[\]] .. char
	return {
		{
			char = char,
			leave = 1,
			at = [[\%#]] .. esc_char,
			filetype = "norg",
		},
		{
			char = char,
			input_after = char,
			except = [[^\s*]] .. esc_char .. [[*\%#]],
			filetype = "norg",
		},
		{
			char = "<BS>",
			at = esc_char .. [[\%#]] .. esc_char,
			delete = 1,
			filetype = "norg",
		},
	}
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
	-- { char = "*", at = [[^\s*\%#]], input = "*<Space>", filetype = "markdown" }, -- Bulleted lists
	{
		char = "]",
		at = [=[\[[^]]*\%#\]]=],
		except = [=[\[@[^]]*\%#\]]=],
		leave = "]",
		input = "(",
		input_after = ")",
		filetype = "markdown",
	}, -- Links

	-- Tasks:
	{
		char = "[",
		input = "[ ]",
		at = [[^\s*[*-]\s*\%#]],
		filetype = "markdown",
	},
	{
		char = "<BS>",
		delete = 3,
		at = [[^\s*[*-]\s*\[.\]\%#]],
		filetype = "markdown",
	},

	-- Handle bold/italic pairs:
	make_markdown_bi_rule("*", true),
	make_markdown_bi_rule("_"),

	-- Neorg Rules:
	-- Bold / Italic:
	neorg_pair("/"),
	neorg_pair("*"),
	neorg_pair("_", "_"),
	neorg_pair("-"),
	neorg_pair("<Bar>", "|"),
	neorg_pair("`"),
	neorg_pair("^"),
	-- neorg_pair(","),
	neorg_pair("$"),
	-- neorg_pair("=", "="),
	-- neorg_pair("+", "+"),

	-- Tasks:
	{
		char = "[",
		input = "[ ]",
		at = [[^\s*-\+\s*\%#]],
		filetype = "norg",
	},
	{
		char = "<BS>",
		delete = 3,
		at = [[^\s*-\+\s*\[.\]\%#]],
		filetype = "norg",
	},

	-- Lists
	{
		char = "<CR>",
		at = [[^\s*\(-\+\)\s*\(\[.\]\)\{0,1\}.*\%#]],
		input = [[<cr>\1 \2]],
		with_submatch = 2,
		filetype = "norg",
	},
	{
		char = "<CR>",
		at = [[^\s*\(\~\+\)\s*.*\%#]],
		input = [[<cr>\1 ]],
		with_submatch = 1,
		filetype = "norg",
	},

	xml_closetag_rules(),
	lua_endwise_rules(),
}
