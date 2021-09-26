--luacheck: globals vim
vim.cmd([[
	packadd plenary.nvim
	packadd null-ls.nvim
]])
local h = require("null-ls.helpers")
local methods = require("null-ls.methods")

local DIAGNOSTICS = methods.internal.DIAGNOSTICS

require("null-ls").config({
	sources = {
		require("null-ls").builtins.formatting.prettier.with({
			extra_args = { "--use-tabs" },
		}),
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.formatting.black,
		require("null-ls").builtins.formatting.fish_indent,
		require("null-ls").builtins.formatting.shfmt,
		require("null-ls").builtins.diagnostics.shellcheck,
		require("null-ls").builtins.diagnostics.luacheck,
		require("null-ls").builtins.diagnostics.eslint,
		require("null-ls").builtins.diagnostics.flake8,
		require("null-ls").builtins.diagnostics.vint,
		h.make_builtin({
			method = DIAGNOSTICS,
			filetypes = { "yaml" },
			generator_opts = {
				command = "yamllint",
				to_stdin = true,
				args = {
					"-f", "parsable", "-"
				},
				format = "line",
				check_exit_code = function(code)
					return code <= 1
				end,
				on_output = h.diagnostics.from_pattern(
					[=[(%w+):(%d+):(%d+): %[(%w+)] (.+) %((.+)%)]=],
					{ "file", "row", "col", "severity", "message", "source" },
					{
						severities = {
							Error = h.diagnostics.severities["error"],
							Warning = h.diagnostics.severities["warning"],
						},
					}
				),
			},
			factory = h.generator_factory,
		}),
		h.make_builtin({
			method = DIAGNOSTICS,
			filetypes = { "html" },
			generator_opts = {
				command = "htmlhint",
				to_stdin = true,
				args = {
					"-f", "json", "stdin"
				},
				format = "json",
				check_exit_code = function(code)
					return code <= 1
				end,
				on_output = function(params)
					local severities = {
						["error"] = 1,
						["warning"] = 2,
						["information"] = 3,
						["hint"] = 4,
					}
					local diagnostics = {}
					for _,diagnostic in ipairs(params.output[1].messages) do
						table.insert(diagnostics, {
							row = diagnostic.line,
							col = diagnostic.col,
							code = diagnostic.rule.id,
							message = diagnostic.message,
							severity = severities[diagnostic.type],
						})
					end
					return diagnostics
				end,
			},
			factory = h.generator_factory,
		}),
	},
})
