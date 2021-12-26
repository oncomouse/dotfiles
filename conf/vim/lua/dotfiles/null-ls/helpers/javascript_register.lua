-- luacheck: globals vim dotfiles
local function eslint_project(utils)
	return utils.root_has_file({
		".eslintrc",
		".eslintrc.js",
		".eslintrc.cjs",
		".eslintrc.yaml",
		".eslintrc.yml",
		".eslintrc.json",
	})
end

local semistandard_types = {
	"formatting",
	"diagnostics",
}

local eslint_types = {
	"formatting",
	"diagnostics",
	"code_actions",
}

local function javascript_register(type)
	return require("null-ls.helpers").conditional(function(utils)
		if not eslint_project(utils) then
			return vim.tbl_contains(semistandard_types, type)
					and require("dotfiles.null-ls.builtins." .. type .. ".semistandard")
				or nil
		end

		local program = vim.fn.executable("eslint_d") == 1 and "eslint_d" or "eslint"
		return vim.tbl_contains(eslint_types, type) and require("null-ls").builtins[type][program] or nil
	end)
end

return javascript_register
