return {
	"nvim-telescope/telescope.nvim",
	version = false,
	cmd = "Telescope",
	keys = {
		{ "<C-H>", "<cmd>Telescope help_tags<cr>", desc = "Help Tags" },
		{ "<C-P>", "<cmd>Telescope find_files<cr>", desc = "Files" },
		{ "<leader>a", "<cmd>Telescope buffers<cr>", desc = "Buffers" },
		{ "<leader>*", "<cmd>Telescope grep_string<cr>", mode = { "n", "v" }, desc = "Search current word" },
	},
	dependencies = {
		{ "nvim-telescope/telescope-ui-select.nvim" },
	},
	init = function()
		vim.ui.select = function(...)
			require("telescope").load_extension("ui-select")
			vim.ui.select(...)
		end
	end,
	opts = function()
		local actions = require("telescope.actions")
		local action_state = require("telescope.actions.state")
		local action_utils = require("telescope.actions.utils")
		local function single_or_multi_selection(prompt_bufnr)
			local current_picker = action_state.get_current_picker(prompt_bufnr)
			local has_multi_selection = (next(current_picker:get_multi_selection()) ~= nil)

			if has_multi_selection then
				local results = {}
				action_utils.map_selections(prompt_bufnr, function(selection)
					table.insert(results, selection[1])
				end)

				-- load the selections into buffers list without switching to them
				for _, filepath in ipairs(results) do
					-- not the same as vim.fn.bufadd!
					vim.cmd.badd({ args = { filepath } })
				end

				require("telescope.pickers").on_close_prompt(prompt_bufnr)

				-- switch to newly loaded buffers if on an empty buffer
				if vim.fn.bufname() == "" and not vim.bo.modified then
					vim.cmd.bwipeout()
					vim.cmd.buffer(results[1])
				end
				return
			end

			-- if does not have multi selection, open single file
			actions.file_edit(prompt_bufnr)
		end
		return {
			defaults = vim.tbl_extend("force", require("telescope.themes").get_ivy(), {
				layout_config = {
					height = vim.o.previewheight,
					preview_cutoff = 80,
					preview_width = 0.4,
				},
				mappings = {
					i = {
						["<esc>"] = actions.close,
						["<C-\\><C-n>"] = function()
							vim.cmd("stopinsert")
						end,
						["<C-d>"] = actions.results_scrolling_down,
						["<C-u>"] = actions.results_scrolling_up,
						["<C-j>"] = actions.results_scrolling_down,
						["<C-k>"] = actions.results_scrolling_up,
						["<C-h>"] = actions.results_scrolling_left,
						["<C-l>"] = actions.results_scrolling_right,
						["<M-j>"] = actions.preview_scrolling_down,
						["<M-k>"] = actions.preview_scrolling_up,
						["<M-h>"] = actions.preview_scrolling_left,
						["<M-l>"] = actions.preview_scrolling_right,
					},
				},
			}),
			pickers = {
				["find_files"] = {
					mappings = {
						i = {
							["<cr>"] = single_or_multi_selection,
						},
						n = {
							["<cr>"] = single_or_multi_selection,
						},
					},
				},
				["help_tags"] = {
					mappings = {
						i = {
							["<cr>"] = actions.select_horizontal,
						},
						n = {
							["<cr>"] = actions.select_horizontal,
						},
					},
				},
			},
			extensions = {
				["ui-select"] = {},
			},
		}
	end,
}
