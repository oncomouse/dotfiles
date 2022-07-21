local ok, npairs = pcall(require, "nvim-autopairs")

if ok then
	local Rule = require("nvim-autopairs.rule")
	-- local cond = require("nvim-autopairs.conds")
	-- local ts_cond = require("nvim-autopairs.ts-conds")
	local utils = require("nvim-autopairs.utils")

	npairs.setup({})

	local function match_vim_regex(line, start_pos, end_pos, regex)
		local re = vim.regex(regex)
		local target = line:sub(start_pos, end_pos)
		return re:match_str(target)
	end

	local function line_matches_vim_regex(regex)
		return function(opts)
			return match_vim_regex(opts.line, 0, #opts.line, regex)
		end
	end

	local function not_line_matches_vim_regex(regex)
		return function(opts)
			return not match_vim_regex(opts.line, 0, #opts.line, regex)
		end
	end

	local function markdown_bold_italic_rule(char)
		local c = vim.fn.fnnameescape(char)
		local regex = c .. c .. "[^".. char .. "]*" .. [[\%#]] .. "[^".. char .. "]*" .. c .. c
		return Rule(char, char, "markdown")
			:with_pair(not_line_matches_vim_regex(regex))
			:with_del(function(opts)
				opts.col = utils.get_cursor()
				return not_line_matches_vim_regex(regex)
			end)
	end

	local function at_pos(col)
		return function(opts)
			return opts.col == col
		end
	end

	npairs.add_rules({
		Rule("*", " ", "markdown")
			:with_pair(at_pos(0)),
		markdown_bold_italic_rule("*"),
		markdown_bold_italic_rule("_"),
	})
end
