local npairs = require("nvim-autopairs")
local endwise = require("nvim-autopairs.ts-rule").endwise
-- VimL rules from lexima.vim
local vim_rules = {}
for _, at in ipairs({
	"fu",
	"fun",
	"func",
	"funct",
	"functi",
	"functio",
	"function",
	"if",
	"wh",
	"whi",
	"whil",
	"while",
	"for",
	"try",
}) do
	table.insert(vim_rules, endwise("^%s*" .. at .. "%W.*$", "end" .. at, "vim", {}))
end
for _, at in ipairs({ "aug", "augroup" }) do
	table.insert(vim_rules, endwise("^%s*" .. at .. "%s+.+$", at .. " END", "vim", {}))
end
npairs.add_rules(vim_rules)

