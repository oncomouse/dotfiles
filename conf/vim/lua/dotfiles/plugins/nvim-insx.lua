return {
	-- "hrsh7th/nvim-insx",
	-- event = "InsertEnter",
	-- init = function()
	-- 	-- Electric Quotes (https://www.gnu.org/software/emacs/manual/html_node/emacs/Quotation-Marks.html)
	-- 	vim.api.nvim_create_user_command(
	-- 		"ToggleElectricQuotes",
	-- 		[[let b:use_electric_quotes = !get(b:, "use_electric_quotes", v:false)]],
	-- 		{}
	-- 	)
	-- end,
	-- config = function()
	-- 	local insx = require("insx")
	-- 	local kit = require("insx.kit")
	--
	-- 	local function add_rule(spec)
	-- 		if not spec.char then
	-- 			return
	-- 		end
	-- 		insx.add(
	-- 			spec.char,
	-- 			insx.with({
	-- 				action = function(ctx)
	-- 					local open = ""
	-- 					local close = ""
	-- 					if not spec.leave and not spec.input then
	-- 						open = open .. spec.char
	-- 					end
	-- 					if spec.leave then
	-- 						local leave = spec.leave
	-- 						if type(leave) == "string" then
	-- 							local s, _ = ctx.match(leave)
	-- 							if s then
	-- 								leave = s
	-- 							end
	-- 						elseif type(leave) == "function" then
	-- 							leave = leave(ctx)
	-- 						end
	-- 						if type(leave) == "number" then
	-- 							open = open .. vim.fn["repeat"]("<Right>", leave)
	-- 						end
	-- 					end
	-- 					if spec.delete then
	-- 						local delete = spec.leave
	-- 						if type(delete) == "string" then
	-- 							local s, _ = ctx.match(delete)
	-- 							if s then
	-- 								delete = s
	-- 							end
	-- 						elseif type(delete) == "function" then
	-- 							delete = delete(ctx)
	-- 						end
	-- 						if type(delete) == "number" then
	-- 							open = open .. vim.fn["repeat"]("<Del>", delete)
	-- 						end
	-- 					end
	-- 					if spec.input then
	-- 						local input = type(spec.input) == "function" and spec.input(ctx) or spec.input
	-- 						if spec.with_submatch then
	-- 							spec.__capture_cache = spec.__capture_cache or {}
	-- 							local line = ctx.text()
	-- 							if spec.__capture_cache[line] then
	-- 								for i, match in ipairs(spec.__capture_cache[line]) do
	-- 									input = input:gsub("\\" .. (i - 1), match)
	-- 								end
	-- 							end
	-- 						end
	-- 						open = open .. input
	-- 					end
	-- 					if spec.input_after then
	-- 						local input_after = type(spec.input_after) == "function" and spec.input_after(ctx)
	-- 							or spec.input_after
	-- 						close = close .. input_after .. ("<Left>"):rep(vim.fn.strchars(input_after, true))
	-- 					end
	-- 					ctx.send(open .. close)
	-- 				end,
	-- 				enabled = function(ctx)
	-- 					if spec.filetype then
	-- 						local ft = kit.to_array(spec.filetype)
	-- 						if not vim.tbl_contains(ft, vim.bo.filetype, {}) then
	-- 							return false
	-- 						end
	-- 					end
	-- 					if spec.enabled then
	-- 						if type(spec.enabled) == "function" then
	-- 							if not spec.enabled(ctx) then
	-- 								return false
	-- 							end
	-- 						end
	-- 						if type(spec.enabled) == "boolean" and spec.enabled == false then
	-- 							return false
	-- 						end
	-- 						if type(spec.enabled) == "nil" then
	-- 							return false
	-- 						end
	-- 					end
	-- 					if spec.at then
	-- 						local at = spec.at
	-- 						if type(at) == "function" then
	-- 							at = at(ctx)
	-- 							if type(at) == "boolean" and at == false then
	-- 								return false
	-- 							end
	-- 							if type(at) == "nil" then
	-- 								return false
	-- 							end
	-- 						end
	-- 						if type(at) == "string" then
	-- 							if not ctx.match(at) then
	-- 								return false
	-- 							elseif spec.with_submatch then
	-- 								local line = ctx.text()
	-- 								local submatch_at = spec.at:gsub([[\%%%#]], "")
	-- 								local captures = vim.fn.matchlist(line, submatch_at)
	-- 								if #captures > 0 then
	-- 									spec.__capture_cache = spec.__capture_cache or {}
	-- 									spec.__capture_cache[line] = captures
	-- 								else
	-- 									return false
	-- 								end
	-- 							end
	-- 						end
	-- 					end
	-- 					if spec.except then
	-- 						local except = spec.except
	-- 						if type(except) == "function" then
	-- 							except = except(ctx)
	-- 							if type(except) == "boolean" and except == true then
	-- 								return false
	-- 							end
	-- 						end
	-- 						if type(except) == "string" and ctx.match(except) then
	-- 							return false
	-- 						end
	-- 					end
	-- 					return true
	-- 				end,
	-- 			}, {})
	-- 		)
	-- 	end
	-- 	require("insx.preset.standard").setup()
	--
	-- 	add_rule({
	-- 		char = "%",
	-- 		input_after = "%",
	-- 	})
	-- 	local function make_endwise_rule(at, ed, ft, syn)
	-- 		return {
	-- 			char = "<CR>",
	-- 			input = "<CR>",
	-- 			input_after = "<CR>" .. ed,
	-- 			at = at,
	-- 			except = [[\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1]] .. ed,
	-- 			filetype = ft,
	-- 			syntax = syn,
	-- 		}
	-- 	end
	--
	-- 	local function make_markdown_bi_rule(char, escape)
	-- 		local esc_char = escape and [[\]] .. char or char
	-- 		add_rule({
	-- 			char = char,
	-- 			input_after = char,
	-- 			filetype = { "text", "markdown" },
	-- 			except = esc_char .. esc_char .. [[.*\%#]] .. esc_char .. esc_char,
	-- 		}) -- Create italic pair
	-- 		add_rule({
	-- 			char = "<Space>",
	-- 			delete = 1,
	-- 			filetype = { "text", "markdown" },
	-- 			at = "^" .. esc_char .. [[\%#]] .. esc_char,
	-- 		}) -- Handle bulleted item
	-- 		add_rule({
	-- 			char = char,
	-- 			at = [[\%#]] .. esc_char,
	-- 			leave = char,
	-- 			filetype = { "text", "markdown" },
	-- 			except = esc_char .. [[\{1\}\%#]],
	-- 		}) -- Leave italic pair
	-- 		add_rule({
	-- 			char = char,
	-- 			at = esc_char .. esc_char .. [[.*\%#]] .. esc_char .. esc_char,
	-- 			leave = 2,
	-- 			filetype = { "text", "markdown" },
	-- 		}) -- Leave bold pair
	-- 		add_rule({
	-- 			char = "<BS>",
	-- 			at = esc_char .. [[\%#]] .. esc_char,
	-- 			delete = char,
	-- 			filetype = { "text", "markdown" },
	-- 		}) -- Delete pair
	-- 	end
	-- 	-- Take a string or list of strings, produce a basic pair
	-- 	local function make_pair(chars, escape, opts)
	-- 		chars = type(chars) == "table" and chars or { chars }
	-- 		opts = opts or {}
	-- 		if type(escape) == "table" then
	-- 			opts = escape
	-- 			escape = false
	-- 		end
	-- 		for _, char in pairs(chars) do
	-- 			local esc_char = escape and [[\]] .. char or char
	-- 			add_rule(vim.tbl_extend("keep", {
	-- 				char = char,
	-- 				input_after = char,
	-- 			}, opts))
	-- 			add_rule(vim.tbl_extend("keep", {
	-- 				char = char,
	-- 				at = [[\%#]] .. esc_char,
	-- 				leave = 1,
	-- 			}, opts))
	-- 			add_rule(vim.tbl_extend("keep", {
	-- 				char = "<BS>",
	-- 				at = esc_char .. [[\%#]] .. esc_char,
	-- 				delete = 1,
	-- 			}, opts))
	-- 		end
	-- 	end
	--
	-- 	-- Lexima Rules
	-- 	-- Correct unbalanced pairs:
	-- 	add_rule({
	-- 		char = "<BS>",
	-- 		at = [[""\%#"]],
	-- 		delete = 1,
	-- 		input = [[<BS><BS>"]],
	-- 		input_after = [["]],
	-- 	})
	-- 	add_rule({
	-- 		char = "<BS>",
	-- 		at = [[((\%#)]],
	-- 		delete = 1,
	-- 		input = [[<BS><BS>(]],
	-- 		input_after = [[)]],
	-- 	})
	-- 	add_rule({
	-- 		char = "<BS>",
	-- 		at = [[[^(](\%#))]],
	-- 		delete = 1,
	-- 	})
	--
	-- 	-- Markdown rules:
	-- 	-- Links:
	-- 	add_rule({
	-- 		char = "]",
	-- 		at = [=[\[[^]]*\%#\]]=],
	-- 		except = [==[\[@[^]]*\%#[^]]*\]]==],
	-- 		leave = "]",
	-- 		input = "(",
	-- 		input_after = ")",
	-- 		filetype = { "text", "markdown" },
	-- 	})
	-- 	-- Blockquotes:
	-- 	add_rule({
	-- 		char = "<BS>",
	-- 		input = "<BS><BS>",
	-- 		at = [[^> \%#]],
	-- 		filetype = { "text", "markdown" },
	-- 	})
	-- 	add_rule({
	-- 		char = "<CR>",
	-- 		at = [[^> .\+\%#$]],
	-- 		input = "<CR>> ",
	-- 		filetype = { "text", "markdown" },
	-- 	})
	-- 	add_rule({
	-- 		char = "<CR>",
	-- 		at = [[^> \%#$]],
	-- 		input = "<BS><BS><CR>",
	-- 		filetype = { "text", "markdown" },
	-- 	})
	-- 	add_rule({
	-- 		char = ">",
	-- 		input = "> ",
	-- 		at = [[^\%#]],
	-- 		filetype = { "text", "markdown" },
	-- 	})
	-- 	-- Unordered Lists:
	-- 	add_rule({
	-- 		char = "<CR>",
	-- 		at = [[^\s*\([*-]\) .*\%#$]],
	-- 		filetype = { "text", "markdown" },
	-- 		with_submatch = true,
	-- 		input = [[<CR>\1 ]],
	-- 		except = [[^\s*\([*-]\) \%#$]],
	-- 	})
	-- 	add_rule({
	-- 		char = "<CR>",
	-- 		at = [[^\s*\([*-]\) \%#$]],
	-- 		filetype = { "text", "markdown" },
	-- 		input = [[<Home><C-O>"_D<CR>]],
	-- 	})
	-- 	add_rule({
	-- 		char = "<BS>",
	-- 		at = [[^\(\s*\)[*-] \%#$]],
	-- 		filetype = { "text", "markdown" },
	-- 		with_submatch = true,
	-- 		input = [[<Home><C-O>"_D\1]],
	-- 	})
	-- 	-- Ordered Lists (including automatic increment):
	-- 	add_rule({
	-- 		char = "<CR>",
	-- 		at = [[^\s*\([0-9]\+\)\..*\%#$]],
	-- 		filetype = { "text", "markdown", "org" },
	-- 		with_submatch = true,
	-- 		input = [[<CR>\1. <Home><C-o>:exec "normal! \<c-a\>" "$"<CR>]],
	-- 		except = [[^\s*\([0-9]\)\. \%#$]],
	-- 	})
	-- 	add_rule({
	-- 		char = "<CR>",
	-- 		at = [[^\s*\([0-9]\+\)\. \%#$]],
	-- 		filetype = { "text", "markdown", "org" },
	-- 		input = [[<Home><C-O>"_D<CR>]],
	-- 	})
	-- 	add_rule({
	-- 		char = "<BS>",
	-- 		at = [[^\(\s*\)[0-9]\+\. \%#$]],
	-- 		filetype = { "text", "markdown", "org" },
	-- 		with_submatch = true,
	-- 		input = [[<Home><C-O>"_D\1]],
	-- 	})
	-- 	-- Tasks:
	-- 	add_rule({
	-- 		char = "[",
	-- 		input = "[ ] ",
	-- 		at = [[^\s*[*-+] \s*\%#]],
	-- 		filetype = { "text", "markdown", "org" },
	-- 	})
	-- 	add_rule({
	-- 		char = "[",
	-- 		input = "[ ] ",
	-- 		at = [[^\s*\d\+\. \s*\%#]],
	-- 		filetype = { "text", "markdown", "org" },
	-- 	})
	-- 	add_rule({
	-- 		char = "<BS>",
	-- 		input = "<BS><BS><BS>",
	-- 		at = [[^\s*[*-+\d]\+\.\{0,1\} \s*\[.\]\%#]],
	-- 		filetype = { "text", "markdown", "org" },
	-- 	})
	-- 	-- Bold/Italic Pairs:
	-- 	make_markdown_bi_rule("*", true)
	-- 	make_markdown_bi_rule("_")
	--
	-- 	-- Org-only rules:
	-- 	make_pair({ "_", "+" }, {
	-- 		filetype = "org",
	-- 	})
	-- 	make_pair({ "~", "*" }, true, {
	-- 		filetype = "org",
	-- 	})
	-- 	-- Don't pair asterisks for headlines:
	-- 	add_rule({
	--
	-- 		char = "*",
	-- 		at = [[^\**\%#]],
	-- 		filetype = "org",
	-- 	})
	-- 	-- Also don't pair if we are actually starting a line with italics:
	-- 	add_rule({
	-- 		char = "*",
	-- 		at = [[^\*\+\w[^*]\+\%#]],
	-- 		filetype = "org",
	-- 	})
	-- 	-- Don't pair pluses in dates:
	-- 	add_rule({
	-- 		char = "+",
	-- 		at = [[<[^>]\+\%#]],
	-- 		filetype = "org",
	-- 	})
	-- 	-- Ordered List:
	-- 	add_rule({
	-- 		char = "+",
	-- 		input = "+ ",
	-- 		at = [[^\s*\%#]],
	-- 		filetype = "org",
	-- 	})
	-- 	add_rule({
	-- 		char = "<BS>",
	-- 		at = [[^\(\s*\)[+-] \%#$]],
	-- 		filetype = "org",
	-- 		with_submatch = true,
	-- 		input = [[<Home><C-O>"_D\1]],
	-- 	})
	-- 	add_rule({
	-- 		char = "<CR>",
	-- 		at = [[^\s*\([+-]\) .*\%#$]],
	-- 		filetype = "org",
	-- 		with_submatch = true,
	-- 		input = [[<CR>\1 ]],
	-- 		except = [[^\s*\([+-]\) \%#$]],
	-- 	})
	-- 	add_rule({
	-- 		char = "<CR>",
	-- 		at = [[^\s*\([+-]\) \%#$]],
	-- 		filetype = "org",
	-- 		input = [[<Home><C-O>"_D]],
	-- 	})
	-- 	-- Handle cookies properly:
	-- 	add_rule({
	-- 		char = "/",
	-- 		input = "/<Right>",
	-- 		at = [==[\[\%#\]]==],
	-- 		filetype = "org",
	-- 	})
	-- 	add_rule({
	-- 		char = "%",
	-- 		input = "%<Right>",
	-- 		at = [==[\[\%#\]]==],
	-- 		filetype = "org",
	-- 	})
	--
	-- 	-- Lua endwise rules:
	-- 	if vim.g.lexima_enable_endwise_rules == 1 then
	-- 		add_rule(make_endwise_rule([[^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {}))
	-- 		add_rule(make_endwise_rule([[^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {}))
	-- 		add_rule(make_endwise_rule([[^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {}))
	-- 	end
	--
	-- 	-- Rules for help files:
	-- 	add_rule({
	-- 		char = "<Bar>",
	-- 		input_after = "<Bar>",
	-- 		filetype = "help",
	-- 	})
	-- 	add_rule({
	-- 		char = "<Bar>",
	-- 		at = [[\%#|]],
	-- 		leave = 1,
	-- 		filetype = "help",
	-- 	})
	-- 	add_rule({
	-- 		char = "<BS>",
	-- 		at = [[|\%#|]],
	-- 		delete = 1,
	-- 		filetype = "help",
	-- 	})
	--
	-- 	-- Links:
	-- 	add_rule({
	-- 		char = "[",
	-- 		at = [[^[+-] \[ \] \%#]],
	-- 		input = "<BS><Left><BS>[",
	-- 		input_after = "]",
	-- 		filetype = "org",
	-- 	}) -- Convert a todo item into a link
	-- 	add_rule({
	-- 		char = "]",
	-- 		at = [==[\%#\]]==],
	-- 		input = "<Right>",
	-- 		filetype = "org",
	-- 	}) -- Leave a square bracket
	--
	-- 	-- Electric Quotes
	-- 	add_rule({
	-- 		char = "`",
	-- 		input = "‘",
	-- 		enabled = function()
	-- 			return vim.b.use_electric_quotes
	-- 		end,
	-- 		except = [[\%#`]],
	-- 	})
	-- 	add_rule({
	-- 		char = "`",
	-- 		at = [[\%#`]],
	-- 		delete = 1,
	-- 	})
	-- 	add_rule({
	-- 		char = "`",
	-- 		at = [[‘\%#]],
	-- 		input = [[<C-o>:call feedkeys(get(b:, "use_electric_quotes", v:false) ? "\<BS\>“" : (g:lexima_enable_basic_rules ? "``\<Left\>" : "`"), "n")<CR>]],
	-- 	})
	-- 	add_rule({
	-- 		char = "'",
	-- 		at = [[[‘“][^‘“]*\%#]],
	-- 		except = [[’\%#]],
	-- 		input = [[<C-o>:call feedkeys(get(b:, "use_electric_quotes", v:false) ? "’" : (g:lexima_enable_basic_rules ? "''\<Left\>" : "'"), "n")<CR>]],
	-- 	})
	-- 	add_rule({
	-- 		char = "'",
	-- 		at = [[’\%#]],
	-- 		input = [[<C-o>:call feedkeys(get(b:, "use_electric_quotes", v:false) ? "\<BS\>”" : (g:lexima_enable_basic_rules ? "''\<Left\>" : "'"), "n")<CR>]],
	-- 	})
	-- 	add_rule({
	-- 		char = "<BS>",
	-- 		at = [[”\%#]],
	-- 		input = [[<C-o>:call feedkeys(get(b:, "use_electric_quotes", v:false) ? "\<BS\>’" : "\<BS\>", "n")<CR>]],
	-- 	})
	-- end,
}
