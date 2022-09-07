local M = {}

local hooks = {}

function M.define_hook(name)
	if hooks[name] == nil then
		hooks[name] = {}
	end
end

function M.add_hook(name, cb)
	assert(hooks[name] ~= nil, "Attempt to attach to undefined hook, " .. name .. "!")
	assert(type(cb) == "function", "Attempt to attach something that is not a function to hook, " .. name .. "!")
	table.insert(hooks[name], cb)
end

function M.run_hook(name, args)
	args = args or {}
	assert(hooks[name] ~= nil, "Attempt to run undefined hook, " .. name .. "!")
	for _, cb in pairs(hooks[name]) do
		cb(args)
	end
end

return M
