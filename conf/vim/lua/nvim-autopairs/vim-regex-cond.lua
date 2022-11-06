local cond = require('nvim-autopairs.cond')
local utils = require('nvim-autopairs.utils')
local log = require('nvim-autopairs._log')

local M = {}

function M.before(regex, length)
    length = length or 1
    if not regex then
        return cond.none()
    end
    ---@param opts CondOpts
    return function(opts)
        log.debug('before_regex')
        if length < 0 then
            length = opts.col
        end
        local str = utils.text_sub_char(opts.line, opts.col - 1, -length)
		local start, _ = vim.regex(regex):match_str(str)
        if start ~= nil then
            return true
        end
        return false
    end
end

function M.after(regex, length)
    length = length or 1
    if not regex then
        return cond.none()
    end
    ---@param opts CondOpts
    return function(opts)
        log.debug('after_regex')
        if length < 0 then
            length = #opts.line
        end
        local str = utils.text_sub_char(opts.line, opts.col, length)
		local start, _ = vim.regex(regex):match_str(str)
		if start ~= nil then
			return true
		end
		return false
    end
end

function M.not_before(regex, length)
    length = length or 1
    if not regex then
        return cond.none()
    end
    ---@param opts CondOpts
    return function(opts)
        log.debug('not_before_regex')
        if length < 0 then
            length = opts.col
        end
        local str = utils.text_sub_char(opts.line, opts.col - 1, -length)
		local start, _ = vim.regex(regex):match_str(str)
		if start == nil then
			return true
		end
		return false
    end
end

function M.not_after(regex, length)
    length = length or 1
    if not regex then
        return cond.none()
    end
    ---@param opts CondOpts
    return function(opts)
        log.debug('not_after_regex')
        if length < 0 then
            length = #opts.line
        end
        local str = utils.text_sub_char(opts.line, opts.col, length)
		local start, _ = vim.regex(regex):match_str(str)
		if start == nil then
			return true
		end
		return false
    end
end

return M
