-- Rule Making Functions:
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
		}, -- Delete pair
	}
end

-- XML-style closetag:
-- local function xml_closetag_rules()
-- 	if vim.g.lexima_disable_closetag == 0 then
-- 		local output = {}
-- 		for _, ft in pairs({ "html", "xml", "javascript", "javascriptreact" }) do
-- 			table.insert(output, { char = "<", input_after = ">", filetype = ft })
-- 			table.insert(output, {
-- 				char = "<BS>",
-- 				at = [[<\%#>]],
-- 				delete = 1,
-- 				filetype = ft,
-- 			})
-- 			table.insert(output, {
-- 				char = ">",
-- 				at = [[<\(\w\+\)[^>]*\%#>]],
-- 				leave = 1,
-- 				input_after = [[</\1>]],
-- 				with_submatch = 1,
-- 				filetype = ft,
-- 			})
-- 		end
-- 		return output
-- 	end
-- 	return nil
-- end

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

-- Lexima Settings:
vim.g.lexima_enable_space_rules = 0
vim.g.lexima_enable_endwise_rules = 1
vim.g.lexima_disable_closetag = 0
vim.g.lexima_no_default_rules = 1

-- Lexima Rules
vim.g.dotfiles_lexima_rules = {
	-- Correct unbalanced pairs:
	{
		char = "<BS>",
		at = [[""\%#"]],
		delete = 1,
		input = [[<BS><BS>"]],
		input_after = [["]],
	},
	{
		char = "<BS>",
		at = [[((\%#)]],
		delete = 1,
		input = [[<BS><BS>(]],
		input_after = [[)]],
	},
	{
		char = "<BS>",
		at = [[[^(](\%#))]],
		delete = 1,
	},
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
		filetype = "markdown",
	},
	{
		char = "<CR>",
		at = [[^> .\+\%#$]],
		input = "<CR>> ",
		filetype = "markdown",
	},
	{
		char = "<CR>",
		at = [[^> \%#$]],
		input = "<BS><BS><CR>",
		filetype = "markdown",
	},
	{
		char = ">",
		input = "> ",
		at = [[^\%#]],
		filetype = "markdown",
	},
	-- Unordered Lists:
	{
		char = "<CR>",
		at = [[^\s*\([*-]\).*\%#$]],
		filetype = "markdown",
		with_submatch = true,
		input = [[<CR>\1 ]],
		except = [[^\s*\([*-]\) \%#$]],
	},
	{
		char = "<CR>",
		at = [[^\s*\([*-]\) \%#$]],
		filetype = "markdown",
		input = "<Home><C-O>Di<CR>",
	},
	{
		char = "<BS>",
		at = [[^\(\s*\)[*-] \%#$]],
		filetype = "markdown",
		with_submatch = true,
		input = [[<Home><C-O>Di\1]],
	},
	-- Ordered Lists (including automatic increment):
	{
		char = "<CR>",
		at = [[^\s*\([0-9]\+\)\..*\%#$]],
		filetype = "markdown",
		with_submatch = true,
		input = [[<CR>\1<Home><C-O><C-A><End>i. ]],
		except = [[^\s*\([0-9]\)\. \%#$]],
	},
	{
		char = "<CR>",
		at = [[^\s*\([0-9]\+\)\. \%#$]],
		filetype = "markdown",
		input = "<Home><C-O>Di<CR>",
	},
	{
		char = "<BS>",
		at = [[^\(\s*\)[0-9]\+\. \%#$]],
		filetype = "markdown",
		with_submatch = true,
		input = [[<Home><C-O>Di\1]],
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
		at = [[^\s*[*-]\s*\[.\]\%#]],
		filetype = "markdown",
	},
	-- Bold/Italic Pairs:
	make_markdown_bi_rule("*", true),
	make_markdown_bi_rule("_"),
	-- {
	-- 	char = "*",
	-- 	input = "* ",
	-- 	at = [[^\%#]],
	-- 	filetype = "markdown",
	-- },
	-- {
	-- 	char = "<CR>",
	-- 	at = [[^\s*\([-*]\) \S\+.*\%#$]],
	-- 	input = [[\<CR\>\1 ]],
	-- 	filetype = "markdown",
	-- 	with_submatch = 1,
	-- },
	-- {
	-- 	char = "<BS>",
	-- 	at = [[^\s*[-*] \%#]],
	-- 	input = [[<BS><BS>]], -- in insx, this can be delete the whole line
	-- 	filetype = "markdown",
	-- },

	-- XML closing tags:
	-- xml_closetag_rules(),

	-- Insert Lua endwise rules:
	lua_endwise_rules(),

	-- Rules for help files:
	{
		char = "<Bar>",
		input_after = "<Bar>",
		filetype = "help",
	},
	{
		char = "<Bar>",
		at = [[\%#|]],
		leave = 1,
		filetype = "help",
	},
	{
		char = "<BS>",
		at = [[|\%#|]],
		delete = 1,
		filetype = "help",
	},
}

return {
	"cohama/lexima.vim", -- Autopairs
	init = function()
		vim.g.lexima_map_escape = ""
	end,
	event = "InsertEnter",
	keys = {
		{
			-- Autoclose mapping:
			"<C-l>",
			"<Plug>(dotfiles-lexima-leave-til-eol)",
			mode = "i",
		},
	},
	config = function()
		vim.fn["lexima#set_default_rules"]()
		local add_rule = vim.fn["lexima#add_rule"]
		vim.keymap.set(
			"i",
			"<Plug>(dotfiles-lexima-leave-til-eol)",
			'<C-r>=lexima#insmode#leave_till_eol("")<CR>',
			{ noremap = true }
		)

		for _, rule in pairs(vim.g.dotfiles_lexima_rules) do
			if type(rule) == "table" then
				if #rule == 0 then
					add_rule(rule)
				else
					for _, r in pairs(rule) do
						add_rule(r)
					end
				end
			end
		end
	end,
}
