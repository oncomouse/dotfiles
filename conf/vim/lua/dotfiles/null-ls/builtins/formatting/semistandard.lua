local h = require("null-ls.helpers")
local methods = require("null-ls.methods")
local FORMATTING = methods.internal.FORMATTING

return h.make_builtin({
	method = FORMATTING,
	filetypes = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
	generator_opts = {
		command = "semistandard",
		args = { "--fix", "--stdin" },
		to_stdin = true,
		ignore_stderr = true,
	},
	factory = h.formatter_factory,
})
