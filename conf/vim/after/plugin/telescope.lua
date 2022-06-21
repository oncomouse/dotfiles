-- {
-- 	"nvim-telescope/telescope.nvim",
-- 	requires = {
-- 		{ "nvim-telescope/telescope-ui-select.nvim" },
-- 		{ "nvim-lua/plenary.nvim", module = "plenary" },
-- 	},
-- },
local ok, tb = pcall(require, "telescope.builtin")
if ok then
	require("telescope").setup({
		pickers = {
			find_files = {
				theme = "ivy",
			},
			buffers = {
				theme = "ivy",
			},
			git_status = {
				theme = "ivy",
			},
		},
		extensions = {
			["ui-select"] = {
				require("telescope.themes").get_ivy({}),
			},
		},
	})
	-- To get ui-select loaded and working with telescope, you need to call
	-- load_extension, somewhere after setup function:
	require("telescope").load_extension("ui-select")
	vim.api.nvim_create_user_command("Files", function(args)
		tb.find_files({
			cwd = args.args == "" and "." or args.args,
		})
	end, {
		complete = "dir",
		force = true,
		nargs = "?",
	})
	vim.api.nvim_create_user_command("Buffers", function()
		tb.buffers()
	end, {
		force = true,
	})
	vim.api.nvim_create_user_command("GitStatus", function()
		tb.git_status()
	end, {
		force = true,
	})
end
