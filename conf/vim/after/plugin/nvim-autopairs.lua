local ok, npairs = pcall(require, "nvim-autopairs")

if ok then
	local t = require("dotfiles.utils.termcode")
	local Rule = require("nvim-autopairs.rule")
	local vim_regex_cond = require("nvim-autopairs.vim-regex-cond")
	local cond = require("nvim-autopairs.conds")
	-- local ts_cond = require("nvim-autopairs.ts-conds")
	local utils = require("nvim-autopairs.utils")

	npairs.setup({})

	local function markdown_bold_italic_rule(char)
		local c = vim.fn.fnameescape(char)
		local regex = c .. c .. "[^" .. char .. "]*" .. [[\%#]] .. "[^" .. char .. "]*" .. c .. c
		return Rule(char, char, "markdown"):with_pair(vim_regex_cond.not_line(regex)):with_del(function(opts)
			opts.col = utils.get_cursor()
			return vim_regex_cond.not_line(regex)
		end)
	end

	local function at_pos(col)
		return function(opts)
			return opts.col == col
		end
	end

	npairs.add_rules({
		markdown_bold_italic_rule("*"),
		markdown_bold_italic_rule("_"),
	})
	npairs.add_rules(require("nvim-autopairs.rules.endwise-lua"))
	npairs.add_rules(require("nvim-autopairs.rules.endwise-ruby"))
	local endwise = require("nvim-autopairs.ts-rule").endwise

	-- Shell Rules:
	npairs.add_rules({
		endwise("%sdo$", "done", "sh", nil),
		endwise("%sdo%s|.*|$", "done", "sh", nil),
		endwise("[%s=]%sif%s.+$", "fi", "sh", nil),
		endwise("[%s=]%scase%s.+$", "esac", "sh", nil),
	})

	-- Add spaces inside brackets
	local brackets = { { "(", ")" }, { "[", "]" }, { "{", "}" } }
	npairs.add_rules({
		Rule(" ", " "):with_pair(function(opts)
			local pair = opts.line:sub(opts.col - 1, opts.col)
			return vim.tbl_contains({
				brackets[1][1] .. brackets[1][2],
				brackets[2][1] .. brackets[2][2],
				brackets[3][1] .. brackets[3][2],
			}, pair)
		end),
	})
	for _, bracket in pairs(brackets) do
		npairs.add_rules({
			Rule(bracket[1] .. " ", " " .. bracket[2])
				:with_pair(function()
					return false
				end)
				:with_move(function(opts)
					return opts.prev_char:match(".%" .. bracket[2]) ~= nil
				end)
				:use_key(bracket[2]),
		})
	end

	-- Blockquotes
	npairs.add_rules({
		Rule(">", " ", "markdown"):with_pair(at_pos(1)):set_end_pair_length(-1),
		Rule("> ", "", "markdown")
			:use_regex(true)
			:with_pair(function(opts)
				return opts.col == 1
			end)
			:with_del(function(opts)
				return opts.col == 2
			end)
			:replace_map_cr(function(opts)
				if opts.line == "> " then
					return [[<BS><BS><CR>]]
				end
				return t([[<C-g>u<C-c>o> ]])
			end),
	})

	-- Unordered Lists
	npairs.add_rules({
		Rule("*", " ", "markdown"):with_pair(at_pos(1)):set_end_pair_length(-1),
		Rule("%* ", "", "markdown")
			:use_regex(true)
			:with_pair(function(opts)
				return opts.col == 1
			end)
			:with_del(function(opts)
				return opts.col == 2
			end)
			:replace_map_cr(function(opts)
				if opts.line == "* " then
					return [[<BS><BS><CR>]]
				end
				return t([[<C-g>u<C-c>o* ]])
			end),
	})

	-- Tasks
	npairs.add_rules({
		Rule("[", " ]", "markdown"):with_pair(function(opts)
			return opts.line:match("^%s*[%*-]%s*")
		end):set_end_pair_length(-2),
		-- Rule("[ ]", "", "markdown")
		-- 	:use_regex(true)
		-- 	:with_pair(cond.none)
		-- 	:with_del(function(opts)
		-- 		return opts.line:match("^%s*[%*-]")
		-- 	end)
	})
end
