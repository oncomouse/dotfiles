local spec_pair = require("mini.ai").gen_spec.pair

vim.b.miniai_config = {
	custom_textobjects = {
		["*"] = spec_pair("*", "*", { type = "greedy" }),
		["_"] = spec_pair("_", "_", { type = "greedy" }),
	},
}

local function s_maker(char)
	local char_esc = vim.pesc(char)
	return {
		input = function()
			local n_char = require("mini.surround").user_input("Number of " .. char .. " to find")
			local many_char = string.rep(char_esc, tonumber(n_char) or 1)
			local find = string.format("%s.-%s", many_char, many_char)
			local extract = string.format("^(%s).*(%s)$", many_char, many_char)
			return { find = find, extract = extract }
		end,
		output = function()
			local n_char = require("mini.surround").user_input("Number of " .. char .. " to output")
			local many_char = string.rep(char, tonumber(n_char) or 1)
			return { left = many_char, right = many_char }
		end,
	}
end
vim.b.minisurround_config = {
	custom_surroundings = {
		["*"] = s_maker("*"),
		["_"] = s_maker("_"),
		["b"] = {
			input = { find = "%*%*.-%*%*", extract = "^(%*%*).*(%*%*)$" },
			output = { left = "**", right = "**" },
		},
		["i"] = {
			input = { find = "%*.-%*", extract = "^(%*).*(%*)$" },
			output = { left = "*", right = "*" },
		},
	},
}
