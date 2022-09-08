local config = require("nvim-ref.config")
local M = {}

-- TODO: cmp source

function M.setup(opts)
	opts = opts or {}
	M.config = config(opts)
	M.hooks = require("nvim-ref.hooks")
	M.hooks.define_hook("setup_done")
	M.hooks.define_hook("add_command")
	M.hooks.define_hook("add_filetype")
	M.filetypes = require("nvim-ref.filetypes").filetypes
	M.commands = { 
		run = require("nvim-ref.commands").run,
	}
	-- Boot up default commands:
	for _,cmd in pairs(M.config.commands) do
		if string.match(cmd, "[.]") then -- Handle accidental subcommand inclusions by the user
			cmd = vim.fn.split(cmd, "\\.")[1]
		end
		local ok, command = pcall(require, "nvim-ref.commands." .. cmd)
		if not ok then
			ok, command = pcall(require, cmd)
			assert(ok, "Could not load default command, " .. cmd .. "!")
		else
			cmd = "nvim-ref.commands." .. cmd
		end
		assert(type(command.setup) == "function", "Could not call .setup() on " .. cmd .. " as it is not a function!")
		command.setup()
	end
	for _,ft in pairs(M.config.filetypes) do
		if string.match(ft, "[.]") then -- Handle accidental subcommand inclusions by the user
			ft = vim.fn.split(ft, "\\.")[1]
		end
		local ok, filetype = pcall(require, "nvim-ref.filetypes." .. ft)
		if not ok then
			ok, filetype = pcall(require, ft)
			assert(ok, "Could not load default filetype, " .. ft .. "!")
		else
			ft = "nvim-ref.filetypes." .. ft
		end
		assert(type(filetype.setup) == "function", "Could not call .setup() on " .. ft .. " as it is not a function!")
		filetype.setup()
	end
	-- TODO: Autocommand(s) to scan for and load bibliography files in document metadata
	M.hooks.run_hook("setup_done")
end

return M
