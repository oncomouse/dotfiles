return {
	"hrsh7th/nvim-insx", -- Autopairs
	event = "InsertEnter",
	init = function()
		-- Lexima Settings:
		vim.g.lexima_map_escape = ""
		vim.g.lexima_enable_space_rules = false
	end,
	config = function()
		require("lexima-insx").setup()
		local add_rule = require("lexima-insx").add_rule

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
				except = "^" .. esc_char .. [[\%#]] .. esc_char,
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

		-- Rules for help files:
		add_rule({
			char = "|",
			input_after = "|",
			filetype = "help",
		})
		add_rule({
			char = "|",
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
