local utils = require("null-ls.utils")
local function eslint_project()
	if
		utils.make_conditional_utils().root_has_file({
			".eslintrc",
			".eslintrc.js",
			".eslintrc.cjs",
			".eslintrc.yaml",
			".eslintrc.yml",
			".eslintrc.json",
		})
	then
		return true
	end
	-- Check for package.json eslintConfig, which is how CRA does it:
	if utils.make_conditional_utils().root_has_file({ "package.json" }) then
		local json_exists, fp = pcall(io.open, utils.get_root() .. "/package.json", "r")
		if json_exists then
			local json_read, package_json = pcall(vim.fn.json_decode, fp:read("*a"))
			fp:close()
			if json_read then
				return vim.fn.has_key(package_json, "eslintConfig") == 1
			end
		end
	end
	return false
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
	if not eslint_project() then
		return vim.tbl_contains(semistandard_types, type)
				and require("dotfiles.null-ls.builtins." .. type .. ".semistandard")
			or nil
	end

	local program = vim.fn.executable("eslint_d") == 1 and "eslint_d" or "eslint"
	return vim.tbl_contains(eslint_types, type) and require("null-ls").builtins[type][program] or nil
end

return javascript_register
