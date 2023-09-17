local M = {}
local function find_root(files)
	local found = vim.fs.find(files, {
		upward = true,
		stop = vim.uv.os_homedir(),
		path = vim.fs.dirname(vim.api.nvim_buf_get_name(0)),
	})
	if #found > 0 then
		return true
	end
	return false
end

function M.eslint_project()
	if
		find_root({
			"eslint.config.js",
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
	local package_json_path = vim.fs.find("package.json", {
		upward = true,
		stop = vim.uv.os_homedir(),
		path = vim.fs.dirname(vim.api.nvim_buf_get_name(0)),
	})
	if #package_json_path > 0 then
		local package_json = vim.json.decode(table.concat(vim.fn.readfile(package_json_path[1]), "\n"))
		if package_json.eslintConfig then
			return true
		end
	end
	return false
end

local function setup_javascript_environment()
	vim.cmd([[compiler eslint]])
	vim.opt_local.formatprg = "prettier --parser babel"
	vim.opt_local.tabstop = 2
	vim.opt_local.shiftwidth = 2
	vim.opt_local.softtabstop = 2
	vim.opt_local.expandtab = true
	vim.opt_local.listchars = vim.opt_local.listchars - "tab:| "
	vim.opt_local.listchars = vim.opt_local.listchars + "multispace:│ "

	vim.api.nvim_create_autocmd("LspAttach", {
		group = vim.api.nvim_create_augroup("dotfiles-javascript-detector", {}),
		callback = function(args)
			local client = vim.lsp.get_client_by_id(args.data.client_id)
			if client and client.name == "null-ls" then
				vim.b.dotfiles_lsp_can_format = true
				if M.eslint_project() then
					require("null-ls").deregister("standardjs")
					require("null-ls").register({
						require("null-ls").builtins.formatting.eslint_d,
						require("null-ls").builtins.diagnostics.eslint_d,
						require("null-ls").builtins.code_actions.eslint_d,
					})
				end
			end
		end,
	})

	if vim.tbl_contains({ "javascript", "javascriptreact" }, vim.bo.filetype) then
		if find_root(".flowconfig") then
			require("dotfiles.lsp").start_server("flow")
		end
	end

	require("dotfiles.lsp.start_server")("tsserver")
end

return setmetatable(M, {
	__call = setup_javascript_environment,
})
