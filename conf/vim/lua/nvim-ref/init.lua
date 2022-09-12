local config = require("nvim-ref.config")
local M = {}

-- Repeated code that loads commands, filetypes, etc.
function load_defaults(obj, t)
	for _, cmd in pairs(obj) do
		local ok, command = pcall(require, "nvim-ref." .. t .. "." .. cmd)
		if not ok then
			ok, command = pcall(require, cmd)
			assert(ok, "Could not load default " .. t:gsub("s$", "") .. ", " .. cmd .. "!")
		else
			cmd = "nvim-ref." .. t .. "." .. cmd
		end
		assert(type(command.setup) == "function", "Could not call .setup() on " .. cmd .. " as it is not a function!")
		command.setup()
	end
end

function M.setup(opts)
	opts = opts or {}
	local open_first_file = false
	M.config = config(opts)
	M.hooks = require("nvim-ref.hooks")
	M.hooks.define("setup_done")
	M.filetypes = require("nvim-ref.filetypes").filetypes
	M.commands = {
		run = require("nvim-ref.commands").run,
	}
	-- Load all our commands when we first encounter a file:
	M.hooks.listen("filetype", function()
		if not open_first_file then
			load_defaults(
				vim.tbl_map(function(cmd)
					if string.match(cmd, "[.]") then
						cmd = vim.fn.split(cmd, "\\.")[1]
					end
					return cmd
				end, M.config.commands),
				"commands"
			)
			-- If cmp is available, register the cmp source
			local ok, cmp = pcall(require, "cmp")
			if ok then
				cmp.register_source("nvim_ref", require("nvim-ref.cmp").new())
			end
			open_first_file = true
		end
	end)
	-- Boot up default filetypes:
	load_defaults(M.config.filetypes, "filetypes")
	M.hooks.trigger("setup_done")
end

return M
