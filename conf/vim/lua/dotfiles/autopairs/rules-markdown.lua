local npairs = require("nvim-autopairs")
local Rule = require("nvim-autopairs.rule")
local cond = require("nvim-autopairs.conds")
local basic = function(...)
	local move_func = cond.move_right or cond.none
	return Rule(...):with_move(move_func()):use_undo(true):with_pair(cond.not_after_regex_check("[%w]"))
end
--Markdown Rules:
npairs.add_rules({
	basic("*", "*", { "markdown", "vimwiki", "rmarkdown", "rmd", "pandoc" }),
	basic("_", "_", { "markdown", "vimwiki", "rmarkdown", "rmd", "pandoc" }),
})
