local hooks = require("nvim-ref.hooks")
local M = {}

local commands = {}
local top_level_commands = {}

hooks.define("add_command")
hooks.listen("add_command", function(args)
	local function add_command(command, ns)
		ns = ns and ns .. "." or ""
		local cmd_id = ns .. command.id
		-- Adding a subcommand directly:
		if ns == "" and string.match(command.id, "%.") then
			local parts = vim.fn.split(command.id, [[\.]])
			ns = vim.fn.join(vim.list_slice(parts, 1, #parts - 1), ".")
			if commands[ns].subcommands == nil then
				commands[ns].subcommands = {}
			end
			table.insert(commands[ns].subcommands, vim.tbl_extend("keep", { id = parts[#parts] }, command))
			cmd_id = command.id
		end
		commands[cmd_id] = command
		if command.subcommands and type(command.subcommands) == "table" then
			for _, subcommand in pairs(command.subcommands) do
				add_command(subcommand, cmd_id)
			end
		end
	end
	if args.id ~= nil then
		args = { args }
	end
	for _, command in pairs(args) do
		add_command(command)
		if not command.id:match("%.") then
			table.insert(top_level_commands, command.id)
		end
	end
end)
-- TODO: End reliance on subcommands

hooks.define("run_command")
hooks.listen("run_command", function(args)
	M.run(args[1], vim.list_slice(args, 2))
end)

-- Define default command:
local created_command = false
hooks.listen("filetype", function()
	if not created_command then
		vim.api.nvim_create_user_command("NvimRef", function(args)
			hooks.trigger("run_command", args.fargs)
		end, {
			force = true,
			complete = M.complete,
			nargs = "*",
		})
		created_command = true
	end
end)

function M.run(command, args)
	if #vim.tbl_keys(commands) == 0 then
		require("nvim-ref.utils.notifications").info("There are no commands loaded; perhaps require('nvim-ref').setup() has not been run?")
		return
	end
	-- Run with no arguments
	if command == nil then
		vim.ui.select(top_level_commands, {
			prompt = "Choose an nvim-ref command: ",
			format_item = function(item)
				return commands[item].name
			end,
		}, function(choice)
			if choice ~= nil then
				M.run(choice, {})
			end
		end)
		return
	end

	assert(commands[command] ~= nil, "Attempt to run unknown command, " .. command .. "!")

	if commands[command].subcommands then -- We still have subcommands, so let the user choose one:
		local choices = commands[command].subcommands
		vim.ui.select(choices, {
			prompt = "Choose a subcommand for " .. command,
			format_item = function(item)
				return item.name
			end,
		}, function(choice)
			if choice ~= nil then
				M.run(command .. "." .. choice.id, {})
			end
		end)
	elseif commands[command].callback then -- Otherwise, we have a calllback function, so we run it:
		assert(
			type(commands[command].callback) == "function",
			string.format("Callback for command, %s, is not a function!", command)
		)
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

return M
