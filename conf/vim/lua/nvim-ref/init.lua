local config = require("nvim-ref.config")
local M = {}

function M.setup(opts)
	opts = opts or {}
	M.config = config(opts)
	require("nvim-ref.command").make_command()
	M.command = require("nvim-ref.command").command
end

return M
