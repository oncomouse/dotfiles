local hooks = require("nvim-ref.hooks")
local M = {}

local commands = {}
local top_level_commands = {}

hooks.define_hook("add_command")
hooks.add_hook("add_command", function(args)
	local function add_command(command, ns)
		ns = ns and ns .. "." or ""
		commands[ns .. command.id] = command
		if command.subcommands and type(command.subcommands) == "table" then
			for _, subcommand in pairs(command.subcommands) do
				add_command(subcommand, ns .. command.id)
			end
		end
	end
	if args.id ~= nil then
		add_command(args)
		table.insert(top_level_commands, args.id)
	else
		for _, command in pairs(args) do
			add_command(command)
			table.insert(top_level_commands, command.id)
		end
	end
end)

function cmd(args)
	if #args.fargs == 0 then
		local choices = top_level_commands
		vim.ui.select(choices, {
			prompt = "Choose an nvim-ref command: ",
			format_item = function(item)
				return commands[item].name
			end,
		}, function(choice)
			M.run(choice, vim.list_slice(args.fargs, 2))
		end)
	else
		M.run(args.fargs[1], vim.list_slice(args.fargs, 2))
	end
end

function M.run(command, args)
	assert(commands[command] ~= nil, "Attempt to run unknown command, " .. command .. "!")

	if commands[command].subcommands then -- We still have subcommands, so let the user choose one:
		-- Choose a subcommand:
		local choices = commands[command].subcommands
		vim.ui.select(choices, {
			prompt = "Choose a subcommand for " .. command,
			format_item = function(item)
				return item.name
			end,
		}, function(choice)
			M.run(command .. "." .. choice.id, {})
		end)
	elseif commands[command].callback then -- Otherwise, we have a calllback function, so we run it:
		commands[command].callback(args)
	else -- Lastly, attempt to load a lua module with our function in it:
		local ok, func = pcall(require, string.format("nvim-ref.commands.%s", command))
		assert(ok, string.format("Could not load module for command, %s.", command))
		assert(type(func) == "function", string.format("Loaded command, %s, is not a function.", command))
		func(args)
	end
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
