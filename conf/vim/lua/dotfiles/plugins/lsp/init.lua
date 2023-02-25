return {
	{
		"neovim/nvim-lspconfig",
		event = "BufReadPre",
		config = function()
			local servers = require("dotfiles.plugins.lsp.servers")

			local handler_no_diagnostics = {
				["textDocument/publishDiagnostics"] = function() end,
			}

			-- Turn on debug-level logging for LSP:
			if vim.g.dotfiles_lsp_debug then
				vim.lsp.set_log_level("trace")
			end

			-- Only attach each server once:
			for lsp, settings in pairs(servers) do
				local pattern = settings.pattern
				if type(pattern) == "table" then
					pattern = table.concat(vim.fn.uniq(vim.fn.sort(pattern)), ",")
				end
				vim.api.nvim_create_autocmd("BufReadPre", {
					pattern = pattern,
					group = "dotfiles-settings",
					once = true,
					callback = function()
						local has_cmp, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
						local function get_server_capabilities(server)
							if has_cmp then
								return cmp_nvim_lsp.default_capabilities
							end
							local capabilities = vim.lsp.protocol.make_client_capabilities()
							if server.snippets then
								capabilities.textDocument.completion.completionItem.snippetSupport = true
								capabilities.textDocument.completion.completionItem.resolveSupport = {
									properties = {
										"documentation",
										"detail",
										"additionalTextEdits",
									},
								}
							end
							return capabilities
						end

						local opts = {
							on_attach = require("dotfiles.plugins.lsp.on_attach"),
							capabilities = get_server_capabilities(servers[lsp]),
						}
						if #vim.tbl_keys(settings) > 0 then
							opts = vim.tbl_extend("keep", opts, settings)
						end
						if not vim.tbl_contains(servers[lsp].provides or {}, "diagnostics") then
							opts.handlers = handler_no_diagnostics
						end
						require("lspconfig")[lsp].setup(opts)
					end,
				})
			end
		end,
	},
	{
		"jose-elias-alvarez/null-ls.nvim",
		ft = {
			"bash",
			"css",
			"fish",
			"html",
			"javascript",
			"javascript.react",
			"json",
			"jsonc",
			"less",
			"lua",
			"markdown",
			"org",
			"py",
			"rb",
			"scss",
			"sh",
			"svelte",
			"tex",
			"typescript",
			"typescript.reac",
			"vim",
			"vue",
			"yaml",
		},
		opts = function()
			local eslint_project = require("dotfiles.null-ls.helpers.eslint_project")
			local b = require("null-ls").builtins
			local has_cmp = pcall(require, "cmp")

			local sources = {
				-- GENERAL PURPOSE
				b.formatting.prettier.with({
					update_on_insert = false,
					filetypes = {
						"css",
						"graphql",
						"handlebars",
						"html",
						"json",
						"jsonc",
						"less",
						"markdown",
						"markdown.mdx",
						"scss",
						"svelte",
					},
					extra_args = { "--use-tabs" },
					prefer_local = "node_modules/.bin",
				}),
				has_cmp and {} or b.completion.luasnip,

				-- YAML
				b.formatting.prettier.with({
					filetypes = {
						"yaml",
					},
					prefer_local = "node_modules/.bin",
				}),

				-- LUA
				b.formatting.stylua,
				b.diagnostics.selene.with({
					cwd = function(_params)
						return vim.fs.dirname(
							vim.fs.find({ "selene.toml" }, { upward = true, path = vim.api.nvim_buf_get_name(0) })[1]
						) or vim.fn.expand("~/.config/selene/") -- fallback value
					end,
				}),

				-- PYTHON
				b.formatting.black.with({
					extra_args = { "-l", "79" }, -- PEP8 line lengths
				}),
				b.formatting.reorder_python_imports,
				b.diagnostics.flake8,

				-- FISH
				b.formatting.fish_indent,

				-- RUBY
				b.formatting.standardrb,
				b.diagnostics.standardrb,

				-- SHELL
				b.formatting.shfmt,
				b.formatting.shellharden,
				b.diagnostics.shellcheck.with({
					diagnostics_format = "#{m} [#{c}]",
				}),
				b.code_actions.shellcheck,

				-- VIML
				b.diagnostics.vint,

				-- HTML
				require("dotfiles.null-ls.builtins.diagnostics.htmlhint"),

				-- MARKDOWN
				require("nvim-ref.null-ls.hover"),
				has_cmp and {} or require("nvim-ref.null-ls.completion"),

				-- JAVASCRIPT
				-- Use standard and prettier for non-eslint projects:
				b.diagnostics.standardjs.with({
					condition = function()
						return not eslint_project()
					end,
				}),
				b.formatting.prettier_standard.with({
					filetypes = {
						"vue",
						"javascript",
						"javascriptreact",
						"typescript",
						"typescriptreact",
					},
					condition = function()
						return not eslint_project()
					end,
				}),
				-- Use eslint_d for eslint projects:
				b.formatting.eslint_d.with({
					condition = function()
						return eslint_project()
					end,
				}),
				b.diagnostics.eslint_d.with({
					condition = function()
						return eslint_project()
					end,
				}),
				b.code_actions.eslint_d.with({
					condition = function()
						return eslint_project()
					end,
				}),
			}

			return {
				on_attach = require("dotfiles.plugins.lsp.on_attach"),
				sources = sources,
			}
		end,
	},
}
