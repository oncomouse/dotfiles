local function eslint_project()
	return false
end

local null_ls_available, utils = pcall(require, "null-ls.utils")
if null_ls_available then
	function eslint_project()
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
end

return eslint_project
