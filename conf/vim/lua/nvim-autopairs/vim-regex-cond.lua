local cond = require("nvim-autopairs.conds")
local utils = require("nvim-autopairs.utils")
local log = require("nvim-autopairs._log")

local M = {}

function M.line(regex)
	if not regex then
		return cond.none()
	end
	return function(opts)
		local start, _ = vim.regex(regex):match_str(opts.line)
		return start ~= nil
	end
end

function M.not_line(regex)
	if not regex then
		return cond.none()
	end
	return function(opts)
		local start, _ = vim.regex(regex):match_str(opts.line)
		return start == nil
	end
end

function M.before(regex, length)
	length = length or 1
	if not regex then
		return cond.none()
	end
	---@param opts CondOpts
	return function(opts)
		log.debug("before_regex")
		if length < 0 then
			length = opts.col
		end
		local str = utils.text_sub_char(opts.line, opts.col - 1, -length)
		local start, _ = vim.regex(regex):match_str(str)
		return start ~= nil
	end
end

function M.after(regex, length)
	length = length or 1
	if not regex then
		return cond.none()
	end
	---@param opts CondOpts
	return function(opts)
		log.debug("after_regex")
		if length < 0 then
			length = #opts.line
		end
		local str = utils.text_sub_char(opts.line, opts.col, length)
		local start, _ = vim.regex(regex):match_str(str)
		return start ~= nil
	end
end

function M.not_before(regex, length)
	length = length or 1
	if not regex then
		return cond.none()
	end
	---@param opts CondOpts
	return function(opts)
		log.debug("not_before_regex")
		if length < 0 then
			length = opts.col
		end
		local str = utils.text_sub_char(opts.line, opts.col - 1, -length)
		local start, _ = vim.regex(regex):match_str(str)
		return start == nil
	end
end

function M.not_after(regex, length)
	length = length or 1
	if not regex then
		return cond.none()
	end
	---@param opts CondOpts
	return function(opts)
		log.debug("not_after_regex")
		if length < 0 then
			length = #opts.line
		end
		local str = utils.text_sub_char(opts.line, opts.col, length)
		local start, _ = vim.regex(regex):match_str(str)
		return start == nil
	end
end

return M
