local h = require("null-ls.helpers")
local methods = require("null-ls.methods")

local DIAGNOSTICS = methods.internal.DIAGNOSTICS

return h.make_builtin({
	method = DIAGNOSTICS,
	filetypes = { "html" },
	generator_opts = {
		command = "htmlhint",
		to_stdin = true,
		args = {
			"-f",
			"json",
			"stdin",
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
			for _, diagnostic in ipairs(params.output[1].messages) do
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
})
