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

	local function not_before_vim_regex(regex)
		return function(opts)
			if match_vim_regex(opts.line, opts.col, #opts.line, regex) then
				return false
			end
		end
	end

	local function not_after_vim_regex(regex)
		return function(opts)
			if match_vim_regex(opts.line, 0, opts.col - 1, regex) then
				return false
			end
		end
	end

	local function markdown_bold_italic_rule(char)
		local regex = vim.fn.fnameescape(char .. char)
		return Rule(char, char, "markdown")
			:with_pair(not_before_vim_regex(regex))
			:with_pair(not_after_vim_regex(regex))
			:with_del(function(opts)
				local _, c = utils.get_cursor()
				opts.col = c
				local before = not_before_vim_regex(regex)(opts)
				local after = not_after_vim_regex(regex)(opts)
				return before or after
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
