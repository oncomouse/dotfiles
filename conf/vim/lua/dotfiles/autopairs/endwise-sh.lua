local npairs = require("nvim-autopairs")
local endwise = require("nvim-autopairs.ts-rule").endwise
-- Shell rules:
npairs.add_rules({
	endwise("^%s*if%W.*$", "fi", { "sh", "zsh" }, {}),
	endwise("^%s*case%W.*$", "esac", { "sh", "zsh" }, {}),
	endwise("^%s*if%W.*$", "fi", { "sh", "zsh" }, {}),
	endwise("%sdo$", "done", { "sh", "zsh" }, {}),
})

