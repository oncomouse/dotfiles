return {
	"oncomouse/lexima.vim", -- Autopairs
	dev = true,
	init = function()
		-- Lexima Settings:
		vim.g.lexima_map_escape = ""
		vim.g.lexima_enable_space_rules = 0
		vim.g.lexima_enable_endwise_rules = 1
		vim.g.lexima_disable_closetag = 0
		vim.g.lexima_no_default_rules = 1
		-- Electric Quotes (https://www.gnu.org/software/emacs/manual/html_node/emacs/Quotation-Marks.html)
		vim.api.nvim_create_user_command(
			"ToggleElectricQuotes",
			[[lua vim.b.use_electric_quotes = not (vim.b.use_electric_quotes or false)]],
			{}
		)
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

		-- Map leave to eol:
		vim.keymap.set(
			"i",
			"<Plug>(dotfiles-lexima-leave-til-eol)",
			'<C-r>=lexima#insmode#leave_till_eol("")<CR>',
			{ noremap = true }
		)

		-- Utilities:
		local add_rule = vim.fn["lexima#add_rule"]

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
				at = "^" .. esc_char .. [[\%#]] .. esc_char,
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
		-- Take a string or list of strings, produce a basic pair
		local function make_pair(chars, escape, opts)
			chars = type(chars) == "table" and chars or { chars }
			opts = opts or {}
			if type(escape) == "table" then
				opts = escape
				escape = false
			end
			for _, char in pairs(chars) do
				local esc_char = escape and [[\]] .. char or char
				add_rule(vim.tbl_extend("keep", {
					char = char,
					input_after = char,
				}, opts))
				add_rule(vim.tbl_extend("keep", {
					char = char,
					at = [[\%#]] .. esc_char,
					leave = 1,
				}, opts))
				add_rule(vim.tbl_extend("keep", {
					char = "<BS>",
					at = esc_char .. [[\%#]] .. esc_char,
					delete = 1,
				}, opts))
			end
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
			except = [==[\[@[^]]*\%#[^]]*\]]==],
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
			filetype = { "text", "markdown", "org" },
			with_submatch = true,
			input = [[<CR>\1. <Home><C-o>:exec "normal! \<c-a\>" "$"<CR>]],
			except = [[^\s*\([0-9]\)\. \%#$]],
		})
		add_rule({
			char = "<CR>",
			at = [[^\s*\([0-9]\+\)\. \%#$]],
			filetype = { "text", "markdown", "org" },
			input = [[<Home><C-O>"_D<CR>]],
		})
		add_rule({
			char = "<BS>",
			at = [[^\(\s*\)[0-9]\+\. \%#$]],
			filetype = { "text", "markdown", "org" },
			with_submatch = true,
			input = [[<Home><C-O>"_D\1]],
		})
		-- Tasks:
		add_rule({
			char = "[",
			input = "[ ] ",
			at = [[^\s*[*-+] \s*\%#]],
			filetype = { "text", "markdown", "org" },
		})
		add_rule({
			char = "[",
			input = "[ ] ",
			at = [[^\s*\d\+\. \s*\%#]],
			filetype = { "text", "markdown", "org" },
		})
		add_rule({
			char = "<BS>",
			input = "<BS><BS><BS>",
			at = [[^\s*[*-+\d]\+\.\{0,1\} \s*\[.\]\%#]],
			filetype = { "text", "markdown", "org" },
		})
		-- Bold/Italic Pairs:
		make_markdown_bi_rule("*", true)
		make_markdown_bi_rule("_")

		-- Org-only rules:
		make_pair({ "_", "+" }, {
			filetype = "org",
		})
		make_pair({ "~", "*" }, true, {
			filetype = "org",
		})
		-- Don't pair asterisks for headlines:
		add_rule({
			char = "*",
			at = [[^\**\%#]],
			filetype = "org",
		})
		-- Also don't pair if we are actually starting a line with italics:
		add_rule({
			char = "*",
			at = [[^\*\+\w[^*]\+\%#]],
			filetype = "org",
		})
		-- Don't pair pluses in dates:
		add_rule({
			char = "+",
			at = [[<[^>]\+\%#]],
			filetype = "org",
		})
		-- Ordered List:
		add_rule({
			char = "+",
			input = "+ ",
			at = [[^\s*\%#]],
			filetype = "org",
		})
		add_rule({
			char = "<BS>",
			at = [[^\(\s*\)[+-] \%#$]],
			filetype = "org",
			with_submatch = true,
			input = [[<Home><C-O>"_D\1]],
		})
		add_rule({
			char = "<CR>",
			at = [[^\s*\([+-]\) .*\%#$]],
			filetype = "org",
			with_submatch = true,
			input = [[<CR>\1 ]],
			except = [[^\s*\([+-]\) \%#$]],
		})
		add_rule({
			char = "<CR>",
			at = [[^\s*\([+-]\) \%#$]],
			filetype = "org",
			input = [[<Home><C-O>"_D]],
		})
		-- Handle cookies properly:
		add_rule({
			char = "/",
			input = "/<Right>",
			at = [==[\[\%#\]]==],
			filetype = "org",
		})
		add_rule({
			char = "%",
			input = "%<Right>",
			at = [==[\[\%#\]]==],
			filetype = "org",
		})
		-- Links:
		add_rule({
			char = "[",
			at = [[^\s*[+-] \[ \] \%#]],
			input = "<BS><Left><BS>[",
			input_after = "]",
			filetype = "org",
		}) -- Convert a todo item into a link
		add_rule({
			char = "]",
			at = [==[\%#\]]==],
			input = "<Right>",
			filetype = "org",
		}) -- Leave a square bracket
		add_rule({
			char = "/",
			at = [==[/\%#]==],
			input = "",
			input_after = "/",
			filetype = "org",
		}) -- Autocomplete italics from //
		add_rule({
			char = "<BS>",
			at = [==[/\%#/]==],
			input = "<BS>",
			delete = 1,
			filetype = "org",
		}) -- Autocomplete italics from //

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

		-- Electric Quotes
		add_rule({
			char = "`",
			input = "‘",
			enabled = function()
				return vim.b.use_electric_quotes
			end,
			except = [[\%#`]],
		})
		add_rule({
			char = "`",
			at = [[\%#`]],
			delete = 1,
		})
		add_rule({
			char = "`",
			at = [[‘\%#]],
			input = "<BS>“",
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = "'",
			at = [[[‘“][^‘“]*\%#]],
			except = [[’\%#]],
			input = "’",
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = "'",
			at = [[’\%#]],
			input = "<BS>”",
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = "<BS>",
			at = [[“\%#]],
			input = "<BS>‘",
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = "<BS>",
			at = [[”\%#]],
			input = "<BS>’",
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = ".",
			at = [[\.\.\%#]],
			input = "<BS><BS>…",
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = "<BS>",
			at = [[…\%#]],
			input = "<BS>..",
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})

		-- Electric dashes and dots
		add_rule({
			char = "-",
			input = "<BS>–",
			at = [[-\%#]],
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = "-",
			input = "<BS>—",
			at = [[–\%#]],
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = "<BS>",
			input = "<BS>–",
			at = [[—\%#]],
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = "<BS>",
			input = "<BS>-",
			at = [[–\%#]],
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = ".",
			input = "<BS><BS>…",
			at = [[..\%#]],
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})
		add_rule({
			char = "<BS>",
			input = "<BS>..",
			at = [[…\%#]],
			enabled = function()
				return vim.b.use_electric_quotes
			end,
		})

		-- XML-style closetag:
		if vim.g.lexima_disable_closetag == 0 then
			add_rule({
				char = "<",
				input_after = ">",
				filetype = { "html", "xml", "javascript", "javascriptreact" },
			})
			add_rule({
				char = "<BS>",
				at = [[<\%#>]],
				delete = 1,
				filetype = { "html", "xml", "javascript", "javascriptreact" },
			})
			add_rule({
				char = ">",
				at = [[<\(\w\+\)[^>]*\%#>]],
				leave = 1,
				input_after = [[</\1>]],
				with_submatch = 1,
				filetype = { "html", "xml", "javascript", "javascriptreact" },
			})
		end
	end,
}
