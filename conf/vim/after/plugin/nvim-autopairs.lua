local ok, npairs = pcall(require, "nvim-autopairs")

if ok then
	local Rule = require("nvim-autopairs.rule")
	local vim_regex_cond = require("nvim-autopairs.vim-regex-cond")
	-- local cond = require("nvim-autopairs.conds")
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
		Rule("*", " ", "markdown"):with_pair(at_pos(0)),
		markdown_bold_italic_rule("*"),
		markdown_bold_italic_rule("_"),
	})
	npairs.add_rules(require("nvim-autopairs.rules.endwise-lua"))
	npairs.add_rules(require("nvim-autopairs.rules.endwise-ruby"))
	local endwise = require("nvim-autopairs.ts-rule").endwise

	-- Shell Rules:
	npairs.add_rules({
		endwise("%sdo$",            "done", "sh", nil),
		endwise("%sdo%s|.*|$",      "done", "sh", nil),
		endwise("[%s=]%sif%s.+$",   "fi",   "sh", nil),
		endwise("[%s=]%scase%s.+$", "esac", "sh", nil),
	})
end
