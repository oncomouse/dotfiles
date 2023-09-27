local Listitem = require("orgmode.treesitter.listitem")
local tree_utils = require("orgmode.utils.treesitter")
local Headline = require("orgmode.treesitter.headline")
local ts_utils = require("nvim-treesitter.ts_utils")
local OrgMappings = require("orgmode.org.mappings")
local TodoState = require("orgmode.objects.todo_state")

-- Used to set the headline to DONE
function TodoState:get_done()
	local first = self.todos.DONE[1]
	return { value = first, type = "DONE", hl = self.hl_map[first] or self.hl_map.DONE }
end

function Listitem:update_cookie(total_child_checkboxes, checked_child_checkboxes)
	local cookie = self:cookie()
	if cookie then
		local new_cookie_val
		local old_cookie_val = vim.treesitter.get_node_text(cookie, 0)
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

	-- If the headline does not have a cookie and does not have a TODO, we can skip this headline:
	local cookie_node = self:cookie()
	local todo_node, current_keyword, is_done = self:todo()
	if cookie_node == nil and todo_node == nil then
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

	-- Detect if there is a TODO state and if the TODO state does not match
	-- the amount of done tasks (TODO -> DONE if done == total; DONE -> TODO if done ~= total)
	if todo_node ~= nil then
		if not is_done and total > 0 and total == done then
			local todo_state = TodoState:new({ current_state = current_keyword })
			self:set_todo(todo_state:get_done().value)
		elseif is_done and total > 0 and total ~= done then
			local todo_state = TodoState:new({ current_state = current_keyword })
			self:set_todo(todo_state:get_todo().value)
		end
	end

	-- Write the new cookie value:
	if cookie_node then
		local old_cookie_val = vim.treesitter.get_node_text(cookie_node, 0)
		local new_cookie_val
		if vim.treesitter.get_node_text(cookie_node, 0):find("%%") then
			new_cookie_val = ("[%d%%]"):format((total == 0 and 0 or done / total) * 100)
		else
			new_cookie_val = ("[%d/%d]"):format(done, total)
		end
		if old_cookie_val ~= new_cookie_val then
			tree_utils.set_node_text(cookie_node, new_cookie_val)
		end
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
	m.action("org_mappings.update_statistics_cookie", { opts = { desc = "Update statistics cookies" } })

local defaults = require("orgmode.config.defaults")
defaults.mappings.org.org_update_statistics_cookie = "<prefix>#"
