return {
	"cohama/lexima.vim", -- Autopairs
	init = function()
		-- Lexima Settings:
		vim.g.lexima_map_escape = ""
		vim.g.lexima_enable_space_rules = 0
		vim.g.lexima_enable_endwise_rules = 1
		vim.g.lexima_disable_closetag = 0
		vim.g.lexima_no_default_rules = 1
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
		-- Load default rules:
		vim.fn["lexima#set_default_rules"]()

		-- Utilities:
		local add_rule = vim.fn["lexima#add_rule"]
		local function make_endwise_rule(at, ed, ft, syn)
			add_rule({
				char = "<CR>",
				input = "<CR>",
				input_after = "<CR>" .. ed,
				at = at,
				except = [[\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1]] .. ed,
				filetype = ft,
				syntax = syn,
			})
		end

		-- Map leave to eol:
		vim.keymap.set(
			"i",
			"<Plug>(dotfiles-lexima-leave-til-eol)",
			'<C-r>=lexima#insmode#leave_till_eol("")<CR>',
			{ noremap = true }
		)
		-- Rule Making Functions:
		local function make_markdown_bi_rule(char, escape)
			local esc_char = escape and [[\]] .. char or char
			add_rule({
				char = char,
				input_after = char,
				filetype = { "text", "markdown" },
				except = esc_char .. esc_char .. [[.*\%#]] .. esc_char .. esc_char,
			}) -- Create italic pair
			add_rule({
				char = "<Space>",
				delete = 1,
				filetype = { "text", "markdown" },
				at = "^" .. esc_char .. [[\%#]] .. esc_char
			}) -- Handle bulleted item
			add_rule({
				char = char,
				at = [[\%#]] .. esc_char,
				leave = char,
				filetype = { "text", "markdown" },
				except = esc_char .. [[\{1\}\%#]],
			}) -- Leave italic pair
			add_rule({
				char = char,
				at = esc_char .. esc_char .. [[.*\%#]] .. esc_char .. esc_char,
				leave = 2,
				filetype = { "text", "markdown" },
			}) -- Leave bold pair
			add_rule({
				char = "<BS>",
				at = esc_char .. [[\%#]] .. esc_char,
				delete = char,
				filetype = { "text", "markdown" },
			}) -- Delete pair
		end

		local function lua_endwise_rules()
			-- Lua endwise rules:
			if vim.g.lexima_enable_endwise_rules == 1 then
				make_endwise_rule([[^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {})
				make_endwise_rule([[^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {})
				make_endwise_rule([[^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {})
			end
			return nil
		end

		-- Lexima Rules
		-- Correct unbalanced pairs:
		add_rule({
			char = "<BS>",
			at = [[""\%#"]],
			delete = 1,
			input = [[<BS><BS>"]],
			input_after = [["]],
		})
		add_rule({
			char = "<BS>",
			at = [[((\%#)]],
			delete = 1,
			input = [[<BS><BS>(]],
			input_after = [[)]],
		})
		add_rule({
			char = "<BS>",
			at = [[[^(](\%#))]],
			delete = 1,
		})

		-- Markdown rules:
		-- Links:
		add_rule({
			char = "]",
			at = [=[\[[^]]*\%#\]]=],
			except = [=[\[@[^]]*\%#\]]=],
			leave = "]",
			input = "(",
			input_after = ")",
			filetype = { "text", "markdown" },
		})
		-- Blockquotes:
		add_rule({
			char = "<BS>",
			input = "<BS><BS>",
			at = [[^> \%#]],
			filetype = { "text", "markdown" },
		})
		add_rule({
			char = "<CR>",
			at = [[^> .\+\%#$]],
			input = "<CR>> ",
			filetype = { "text", "markdown" },
		})
		add_rule({
			char = "<CR>",
			at = [[^> \%#$]],
			input = "<BS><BS><CR>",
			filetype = { "text", "markdown" },
		})
		add_rule({
			char = ">",
			input = "> ",
			at = [[^\%#]],
			filetype = { "text", "markdown" },
		})
		-- Unordered Lists:
		add_rule({
			char = "<CR>",
			at = [[^\s*\([*-]\) .*\%#$]],
			filetype = { "text", "markdown" },
			with_submatch = true,
			input = [[<CR>\1 ]],
			except = [[^\s*\([*-]\) \%#$]],
		})
		add_rule({
			char = "<CR>",
			at = [[^\s*\([*-]\) \%#$]],
			filetype = { "text", "markdown" },
			input = [[<Home><C-O>"_D<CR>]],
		})
		add_rule({
			char = "<BS>",
			at = [[^\(\s*\)[*-] \%#$]],
			filetype = { "text", "markdown" },
			with_submatch = true,
			input = [[<Home><C-O>"_D\1]],
		})
		-- Ordered Lists (including automatic increment):
		add_rule({
			char = "<CR>",
			at = [[^\s*\([0-9]\+\)\..*\%#$]],
			filetype = { "text", "markdown" },
			with_submatch = true,
			input = [[<CR>\1. <Home><C-o>:exec "normal! \<c-a\>" "$"<CR>]],
			except = [[^\s*\([0-9]\)\. \%#$]],
		})
		add_rule({
			char = "<CR>",
			at = [[^\s*\([0-9]\+\)\. \%#$]],
			filetype = { "text", "markdown" },
			input = [[<Home><C-O>"_D<CR>]],
		})
		add_rule({
			char = "<BS>",
			at = [[^\(\s*\)[0-9]\+\. \%#$]],
			filetype = { "text", "markdown" },
			with_submatch = true,
			input = [[<Home><C-O>"_D\1]],
		})
		-- Tasks:
		add_rule({
			char = "[",
			input = "[ ]",
			at = [[^\s*[*-0-9]\+\.\{0,1\} \s*\%#]],
			filetype = { "text", "markdown" },
		})
		add_rule({
			char = "<BS>",
			input = "<BS><BS><BS>",
			at = [[^\s*[*-\d]\+\.\{0,1\} \s*\[.\]\%#]],
			filetype = { "text", "markdown" },
		})
		-- Bold/Italic Pairs:
		make_markdown_bi_rule("*", true)
		make_markdown_bi_rule("_")

		-- Lua endwise rules:
		lua_endwise_rules()

		-- Rules for help files:
		add_rule({
			char = "<Bar>",
			input_after = "<Bar>",
			filetype = "help",
		})
		add_rule({
			char = "<Bar>",
			at = [[\%#|]],
			leave = 1,
			filetype = "help",
		})
		add_rule({
			char = "<BS>",
			at = [[|\%#|]],
			delete = 1,
			filetype = "help",
		})
	end,
}
