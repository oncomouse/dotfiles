local h = require("null-ls.helpers")
local methods = require("null-ls.methods")

local DIAGNOSTICS = methods.internal.DIAGNOSTICS
return h.make_builtin({
	method = DIAGNOSTICS,
	filetypes = { "yaml" },
	generator_opts = {
		command = "yamllint",
		to_stdin = true,
		args = {
			"-f",
			"parsable",
			"-",
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
})
