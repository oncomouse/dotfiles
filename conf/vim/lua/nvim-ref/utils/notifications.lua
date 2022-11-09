local M = {}

function M.info(msg)
	vim.notify("[nvim-ref] " .. msg, vim.log.levels.INFO)
end

function M.warn(msg)
	vim.notify("[nvim-ref] " .. msg, vim.log.levels.WARN)
end

function M.error(msg)
	vim.notify("[nvim-ref] " .. msg, vim.log.levels.ERROR)
end
return M
