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

M.local_packages = {}

local function generate_local_packages()
	if #M.local_packages ~= 0 then
		return
	end
	local registry = require("mason-registry")
	if registry.add_package ~= nil then
		registry.add_package("rubocop", "dotfiles.plugins.mason.registry.rubocop")
		registry.add_package("selene", "dotfiles.plugins.mason.registry.selene")
		registry.add_package("shellharden", "dotfiles.plugins.mason.registry.shellharden")
		registry.add_package("standardrb", "dotfiles.plugins.mason.registry.standardrb")
		table.insert(M.local_packages, "done")
	else
		M.local_packages["rubocop"] = require("dotfiles.plugins.mason.registry.rubocop")
		M.local_packages["selene"] = require("dotfiles.plugins.mason.registry.selene")
		M.local_packages["shellharden"] = require("dotfiles.plugins.mason.registry.shellharden")
		M.local_packages["standardrb"] = require("dotfiles.plugins.mason.registry.standardrb")
	end
end

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

local function configure_mason()
	require("mason").setup()
end

function M.install_tools()
	configure_mason()
	generate_local_packages()
	vim.tbl_map(install_maybe, M.packages)
end

function M.install_lsp()
	configure_mason()
	local servers = require("dotfiles.nvim-lsp.servers")
	require("mason-lspconfig").setup({
		ensure_installed = vim.tbl_keys(servers),
	})
end

return M
