local npairs = require("nvim-autopairs")
local endwise = require("nvim-autopairs.ts-rule").endwise
local lua_rules = require("nvim-autopairs.rules.endwise-lua")
-- Why no loops in nvim-autopairs builtins?
table.insert(
	lua_rules,
	endwise("do$", "end", "lua", {
		"for_in_statement",
		"while_statement",
	})
)

npairs.add_rules(lua_rules)
