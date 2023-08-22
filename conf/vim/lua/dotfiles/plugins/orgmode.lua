return {
	"nvim-orgmode/orgmode",
	dev = false,
	ft = "org",
	keys = {
		{ "<leader>oa", "<Cmd>lua require('orgmode').action('agenda.prompt')<CR>", desc = "org agenda" },
		{ "<leader>oc", "<Cmd>lua require('orgmode').action('capture.prompt')<CR>", desc = "org capture" },
	},
	opts = {
		org_agenda_files = {
			"~/SeaDrive/My Libraries/Todo/**/*.org",
		},
		org_default_notes_file = "~/SeaDrive/My Libraries/Todo/inbox.org",
		org_indent_mode = "noindent",
		org_capture_templates = {
			t = { description = "Todo Item", template = "* TODO %?\n  %u" },
			p = { description = "Paste Todo Item", template = "* TODO %x%?\n  %u" },
		},
		org_tags_column = 0,
		mappings = {
			org = {
				org_toggle_checkbox = { "<leader>o<space>", "gtd" },
				org_return = false,
			},
		},
	},
	config = function(_, opts)
		-- Just export the stupid class:
		local Date = getmetatable(require("orgmode.objects.date").now())
		local Listitem = require("orgmode.treesitter.listitem")
		local tree_utils = require("orgmode.utils.treesitter")
		local Headline = require("orgmode.treesitter.headline")
		local ts_utils = require("nvim-treesitter.ts_utils")
		local ts = require("orgmode.treesitter.compat")
		local OrgMappings = require("orgmode.org.mappings")

		-- OVERRIDES
		-- Support for proper date ranges
		local time_format = "%H:%M"
		local date_format = "%Y-%m-%d"
		function Date:to_string()
			local format = date_format
			if self.dayname then
				format = format .. " %a"
			end

			local date = os.date(format, self.timestamp)
			if not self.date_only then
				date = date .. " " .. self:format_time()
			end

			if #self.adjustments > 0 then
				date = date .. " " .. table.concat(self.adjustments, " ")
			end

			return date
		end
		function Date:format_time()
			if self.date_only then
				return ""
			end
			local t = self:format(time_format)
			if self.timestamp_end then
				t = t .. "-" .. os.date(time_format, self.timestamp_end)
			elseif
				self.related_date_range
				and self.related_date_range.is_date_range_end
				and self:is_same(self.related_date_range, "day")
			then
				t = t .. "-" .. os.date(time_format, self.related_date_range.timestamp)
			end
			return t
		end
		-- End support for proper date ranges

		-- Support for org-update-statistics-cookie
		function Listitem:update_cookie(total_child_checkboxes, checked_child_checkboxes)
			local cookie = self:cookie()
			if cookie then
				local new_cookie_val
				local old_cookie_val = ts.get_node_text(cookie, 0)
				if old_cookie_val:find("%%") then
					new_cookie_val = ("[%d%%]"):format((#checked_child_checkboxes / #total_child_checkboxes) * 100)
				else
					new_cookie_val = ("[%d/%d]"):format(#checked_child_checkboxes, #total_child_checkboxes)
				end
				if old_cookie_val ~= new_cookie_val then
					tree_utils.set_node_text(cookie, new_cookie_val)
				end
			end
		end

		local function count_checkboxes(container)
			local total = 0
			local done = 0
			local total_boxes = vim.tbl_filter(function(box)
				return box:match("%[.%]")
			end, container)
			local checked_boxes = vim.tbl_filter(function(box)
				return box:match("%[%w%]")
			end, total_boxes)
			total = total + #total_boxes
			done = done + #checked_boxes
			return total, done
		end

		function Headline:update_cookie(list_node)
			local done = 0
			local total = 0
			local cookie = self:cookie()
			if not cookie then
				return
			end
			if type(list_node) == "userdata" then
				total, done = count_checkboxes(self:child_checkboxes(list_node))
			else
				local node_type
				-- Parse the children of the headline's parent section for child headlines and lists:
				for _, node in pairs(ts_utils.get_named_children(tree_utils.find_parent_type(self.headline, "section"))) do
					-- The child is a list:
					if node:type() == "body" and node:child(0):type() == "list" then
						if not node_type then
							node_type = "list"
						end
						if node_type == "list" then
							local t, d = count_checkboxes(self:child_checkboxes(node:child(0)))
							total = total + t
							done = done + d
						end
					end
					-- The child is a section:
					if node:type() == "section" and node:child(0):type() == "headline" then
						if not node_type then
							node_type = "headline"
						end
						if node_type == "headline" then
							local hl = Headline:new(node:child(0))
							local _, word, is_done = hl:todo()
							if word ~= nil then
								total = total + 1
							end
							if is_done then
								done = done + 1
							end
						end
					end
				end
			end
			local old_cookie_val = ts.get_node_text(cookie, 0)
			local new_cookie_val
			if ts.get_node_text(cookie, 0):find("%%") then
				new_cookie_val = ("[%d%%]"):format((total == 0 and 0 or done / total) * 100)
			else
				new_cookie_val = ("[%d/%d]"):format(done, total)
			end
			if old_cookie_val ~= new_cookie_val then
				tree_utils.set_node_text(cookie, new_cookie_val)
			end
		end

		function OrgMappings:update_statistics_cookie()
			local current_node = tree_utils.get_node_at_cursor()
			-- Wrap this with pcall() because it throws errors sometimes:
			local is_ok, nearest_list = pcall(tree_utils.find_parent_type, current_node, "list")
			if
				is_ok
				and type(nearest_list) == "userdata"
				and nearest_list:parent()
				and nearest_list:parent():type() == "listitem"
			then
				nearest_list = nearest_list:parent()
			else
				nearest_list = nil
			end
			local nearest_headline = tree_utils.closest_headline()

			local target
			if nearest_list then
				target = Listitem:new(nearest_list)
				target:update_checkbox("children")
			elseif nearest_headline then
				target = Headline:new(nearest_headline)
				target:update_cookie()
			end
		end

		local m = require("orgmode.config.mappings.map_entry")
		local mappings = require("orgmode.config.mappings")
		mappings.org.org_update_statistics_cookie =
			m.action("org_mappings.update_statistics_cookie", { opts = { desc = "org promote headline" } })

		local defaults = require("orgmode.config.defaults")
		defaults.mappings.org.org_update_statistics_cookie = "<prefix>#"
		-- End support for org-update-statistics-cookie
		-- END OVERRIDES

		require("orgmode").setup_ts_grammar()
		require("orgmode").setup(opts)
		if vim.bo.filetype == "org" then
			require("orgmode").reload(vim.fn.expand("<afile>:p"))
		end
	end,
}
