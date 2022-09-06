local M = {}
local subcommands = {
	{ "insert-key", "Insert a Citation Key" },
	{ "insert-citation", "Insert a Citation Reference" },
	"edit",
}

function cmd(args)
	local a = vim.fn.split(args.args, " ")
	if #a == 0 then
		local choices = M.complete(".")
		vim.ui.select(choices, {
			prompt = "Choose an nvim-ref command: ",
			format_item = function(item)
				if type(item) == "table" then
					return item[2]
				end
			end,
		}, function(choice)
			M.command(type(choice) == "table" and choice[1] or choice, {})
		end)
	else
		M.command(a[1], vim.list_slice(a, 2))
	end
end

function M.command(subcommand, args)
	args = args or {}
	if subcommand == "insert-key" then
		require("nvim-ref.select").insert(type(args) == "string" and args or args[1])
	elseif subcommand == "insert-citation" then
		require("nvim-ref.select").insert(type(args) == "string" and args or args[1], { type = "citation" })
	end
end

function M.complete(typed)
	typed = typed or "."
	return vim.tbl_map(function(x)
		return type(x) == "string" and x or x[1]
	end, vim.tbl_filter(function(x)
		return string.match((type(x) == "string" and x or x[1]), "^" .. typed)
	end, subcommands))
end

function M.make_command()
	vim.api.nvim_create_user_command("NvimRef", cmd, {
		force = true,
		complete = M.complete,
		nargs = "*",
	})
end

return M
