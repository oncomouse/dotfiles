local M = {}
M.packages = {
	"black",
	"flake8",
	"prettier",
	"rubocop",
	"selene",
	"shellcheck",
	"shellharden",
	"shfmt",
	"standardrb",
	"stylua",
	"vint",
}

local function install_maybe(pkg)
	if vim.fn.executable(pkg) ~= 1 then
		local in_registry, spec = pcall(require("mason-registry").get_package, pkg)
		local installed = false
		if in_registry then
			installed = spec:is_installed()
		else
			spec = M.local_packages[pkg]
			installed = spec == nil and false or vim.fn.isdirectory(spec:get_install_path()) == 1
		end
		if not installed then
			spec:install()
		end
	end
end

local run_setup = false

local function configure_mason()
	if not run_setup then
		require("mason").setup()
		run_setup = true
	end
end

function M.install_tools()
	configure_mason()
	vim.tbl_map(install_maybe, M.packages)
end

function M.install_lsp()
	configure_mason()
	local servers = require("dotfiles.plugins.nvim-lspconfig.servers")
	require("mason-lspconfig").setup({
		ensure_installed = vim.tbl_keys(servers),
	})
end

return M
