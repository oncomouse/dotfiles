return {
	"mfussenegger/nvim-lint",
	config = function()
		local function get_cwd()
			if not vim.b.nvim_lint_cwd then
				if vim.bo.filetype == "lua" then
					vim.b.nvim_lint_cwd = vim.fs.dirname(
						vim.fs.find({ "selene.toml" }, { upward = true, path = vim.api.nvim_buf_get_name(0) })[1]
					) or vim.fn.expand("~/.config/selene/") -- fallback value
				else
					vim.b.nvim_lint_cwd = vim.fs.dirname(vim.fs.dirname(vim.fs.normalize(vim.api.nvim_buf_get_name(0))))
				end
			end
			return vim.b.nvim_lint_cwd
		end
		vim.api.nvim_create_autocmd({ "BufWritePost", "InsertLeave", "TextChanged" }, {
			group = vim.api.nvim_create_augroup("dotfiles-nvim-lint", {}),
			callback = function()
				require("lint").try_lint(nil, {
					cwd = get_cwd(),
				})
			end,
		})
		require("lint").linters.htmlhint = {
			cmd = "htmlhint",
			stdin = true,
			args = {
				"-f",
				"json",
				"stdin",
			},
			parser = function(output, _)
				local severities = {
					error = vim.diagnostics.severity.ERROR,
					warning = vim.diagnostics.severity.WARN,
					information = vim.diagnostics.severity.INFO,
					hint = vim.diagnostics.severity.HINT,
				}

				if vim.trim(output) == "" or output == nil then
					return {}
				end

				if not vim.startswith(output, "{") then
					return {}
				end

				local decoded = vim.json.decode(output)
				local diagnostics = {}
				for _, diagnostic in decoded[1].messages do
					table.insert(diagnostics, {
						lnum = diagnostic.line,
						end_lnum = diagnostic.line,
						col = diagnostic.col,
						end_col = diagnostic.col,
						message = diagnostic.message,
						severity = severities[diagnostic.type],
						code = diagnostic.rule.id,
					})
				end
				return diagnostics
			end,
		}
		require("lint").linters_by_ft = {
			html = {
				"htmlhint",
			},
			lua = {
				"selene",
			},
			python = {
				"flake8",
			},
			sh = {
				"shellcheck",
			},
			viml = {
				"vint",
			},
		}
	end,
}
