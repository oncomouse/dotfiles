local h = require("null-ls.helpers")
local methods = require("null-ls.methods")

local DIAGNOSTICS = methods.internal.DIAGNOSTICS

return h.make_builtin({
	method = DIAGNOSTICS,
	filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
	generator_opts = {
		command = "semistandard",
		to_stdin = true,
		ignore_stderr = true,
		args = {
			"--stdin",
		},
		format = "line",
		check_exit_code = function(code)
			return code <= 1
		end,
		on_output = h.diagnostics.from_pattern([=[<text>:(%d+):(%d+): (.+)]=], { "row", "col", "message" }, {
			severities = {
				_fallback = h.diagnostics.severities["warning"],
			},
		}),
	},
	factory = h.generator_factory,
})
