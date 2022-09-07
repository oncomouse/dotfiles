local hooks = require("nvim-ref.hooks")
local M = {}

local commands = {}

hooks.define_hook("add_command")
hooks.add_hook("add_command", function(args)
	local function add_command(command)
		commands[command.id] = command
	end
	if args.id ~= nil then
		add_command(args)
	else
		for _, command in pairs(args) do
			add_command(command)
		end
	end
end)

function cmd(args)
	if #args.fargs == 0 then
		local choices = M.complete(".")
		vim.ui.select(choices, {
			prompt = "Choose an nvim-ref command: ",
			format_item = function(item)
				return commands[item].name
			end,
		}, function(choice)
			cmd({
				fargs = { choice },
			})
		end)
	else
		local command, subcommand = unpack(vim.fn.split(args.fargs[1], "\\."))
		assert(commands[command] ~= nil, "Attempt to run unknown command, " .. command .. "!")
		if subcommand == nil and #args.fargs > 1 then
			subcommand = args.fargs[2]
		end
		if commands[command].subcommands ~= nil then
			if subcommand ~= nil then
				for _, sc in pairs(commands[command].subcommands) do
					if sc.id == subcommand then
						M.run(args.fargs[1] .. "." .. subcommand, vim.list_slice(args.fargs, 3))
						return
					end
				end
				error(string.format("Subcommand, %s.%s, is not found", command, subcommand))
			else -- Choose a subcommand:
				local choices = commands[args.fargs[1]].subcommands
				vim.ui.select(choices, {
					prompt = "Choose a subcommand for " .. args.fargs[1],
					format_item = function(item)
						return item.name
					end,
				}, function(choice)
					M.run(args.fargs[1] .. "." .. choice.id, {})
				end)
			end
		else -- There are no subcommands, so pass evertything:
			M.run(args.fargs[1], vim.list_slice(args.fargs, 2))
		end
	end
end

function M.dispatcher(command, subcommand, args)
	local target = command
	if subcommand ~= nil then
		target = string.format("%s.%s", command, subcommand)
	end
	local ok, func = pcall(require, string.format("nvim-ref.commands.%s", target))
	assert(ok, string.format("Could not load command, %s, ", target))
	assert(type(func) == "function", string.format("Loaded command, %s, is not a function.", target))
	func(args)
end

function M.run(command, args)
	local subcommand = nil
	command, subcommand = unpack(vim.fn.split(command, "\\."))
	assert(commands[command] ~= nil, "Attempt to run unknown command, " .. command .. "!")
	M.dispatcher(command, subcommand, args)
end

function M.complete(typed)
	typed = typed or "."
	return vim.tbl_filter(function(x)
		return string.match(x, "^" .. typed)
	end, vim.tbl_keys(commands))
end

function M.make_command()
	vim.api.nvim_create_user_command("NvimRef", cmd, {
		force = true,
		complete = M.complete,
		nargs = "*",
	})
end

return M
