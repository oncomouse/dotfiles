local config = require("nvim-ref.config")
local M = {}

function M.setup(opts)
	opts = opts or {}
	M.config = config(opts)
end

return M
