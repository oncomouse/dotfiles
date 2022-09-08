local config = require("nvim-ref.config")
local M = {}

-- TODO: cmp source

function M.setup(opts)
	opts = opts or {}
	M.config = config(opts)
	M.hooks = require("nvim-ref.hooks")
	M.hooks.define_hook("setup_done")
	M.commands = { 
		run = require("nvim-ref.commands").run,
	}
	-- Boot up default commands:
	for _,cmd in pairs(M.config.commands) do
		if string.match(cmd, "[.]") then -- Handle accidental subcommand inclusions by the user
			cmd = vim.fn.split(cmd, "\\.")[1]
		end
		local ok = pcall(require, "nvim-ref.commands." .. cmd)
		assert(ok, "Could not load default command, nvim-ref.commands." .. cmd .. "!")
	end
	-- TODO: Autocommand(s) to scan for and load bibliography files in document metadata
	M.hooks.run_hook("setup_done")
end

return M
