local config = require("nvim-ref.config")
local M = {}

function M.setup(opts)
	opts = opts or {}
	M.config = config(opts)
	M.hooks = require("nvim-ref.hooks")
	M.hooks.define_hook("setup_done")
	require("nvim-ref.commands").make_command()
	M.commands = { 
		invoker = require("nvim-ref.commands").dispatcher,
	}
	-- Boot up default commands:
	for _,cmd in pairs(M.config.commands) do
		local ok = pcall(require, "nvim-ref.commands." .. cmd)
		assert(ok, "Could not load default command, nvim-ref.commands." .. cmd .. "!")
	end
	M.hooks.run_hook("setup_done")
end

return M
