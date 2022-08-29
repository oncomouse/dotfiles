local function lexima_rules()
	local add_rule = vim.fn["lexima#add_rule"]
	-- Markdown rules:
	-- add_rule({ char = "*", at = [[^\s*\%#]], input = "*<Space>", filetype = "markdown" }) -- Bulleted lists
	add_rule({
		char = "]",
		at = [=[\[[^]]*\%#\]]=],
		except = [=[\[@[^]]*\%#\]]=],
		leave = "]",
		input = "(",
		input_after = ")",
		filetype = "markdown",
	}) -- Links

	-- Handle bold/italic pairs:
	local function make_markdown_bi_rule(char, escape)
		local esc_char = escape and [[\]] .. char or char
		add_rule({
			char = char,
			input_after = char,
			filetype = "markdown",
			except = [[^]] .. esc_char .. [[\{0,1\}\%#]],
		}) -- Create italic pair
		add_rule({
			char = char,
			at = [[\%#]] .. esc_char,
			leave = char,
			filetype = "markdown",
			except = esc_char .. [[\{1\}\%#]],
		}) -- Leave italic pair
		add_rule({
			char = char,
			at = esc_char .. esc_char .. [[.\+\%#]] .. esc_char,
			leave = 2,
			filetype = "markdown",
			except = esc_char .. [[\{1\}\%#]],
		}) -- Leave bold pair
		add_rule({
			char = "<BS>",
			at = esc_char .. [[\%#]] .. esc_char,
			delete = char,
			filetype = "markdown",
		}) -- Delete pair
	end
	make_markdown_bi_rule("*", true)
	make_markdown_bi_rule("_")

	add_rule({
		char = "/",
		input_after = "/",
		filetype = "norg",
	})
	add_rule({
		char = "*",
		input_after = "*",
		filetype = "norg",
	})
	add_rule({
		char = "<BS>",
		at = [[\*]] .. [[\%#]] .. [[\*]],
		delete = "*",
		filetype = "norg",
	}) -- Delete pair
	add_rule({
		char = "<BS>",
		at = [[\/]] .. [[\%#]] .. [[\/]],
		delete = "/",
		filetype = "norg",
	}) -- Delete pair

	-- XML-style closetag:
	if vim.g.lexima_disable_closetag == 0 then
		vim.api.nvim_create_autocmd("FileType", {
			pattern = "html,xml,javascript,javascriptreact",
			group = "dotfiles-settings",
			callback = function()
				add_rule({ char = "<", input_after = ">" })
				add_rule({
					char = ">",
					at = [[<\(\w\+\)\%#>]],
					leave = 1,
					input_after = [[</\1>]],
					with_submatch = 1,
				})
			end,
			desc = "Rules for auto-closing XML-style tags",
		})
	end

	-- Lua endwise rules:
	if vim.g.lexima_enable_endwise_rules == 1 then
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
		add_rule(make_endwise_rule([[^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {}))
		add_rule(
			make_endwise_rule([[^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {})
		)
		add_rule(
			make_endwise_rule([[^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {})
		)
	end

	-- Autoclose mapping:
	vim.keymap.set("i", "<Plug>(dotfiles-lexima)", '<C-r>=lexima#insmode#leave_till_eol("")<CR>', { noremap = true })
	vim.keymap.set("i", "<C-l>", "<Plug>(dotfiles-lexima)", { silent = true })
end

return lexima_rules
