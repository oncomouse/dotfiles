-- Set indent:
vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true
-- Turn on conceal:
vim.opt_local.conceallevel = 2
-- Spell
vim.opt_local.spell = true

-- Do a better >>/<< for my editing style
local translation = {
	["<<"] = "promote",
	[">>"] = "demote",
}
local function do_demote_promote(symbol)
	local line = vim.api.nvim_get_current_line()
	if line:match("^%*+") then
		require("orgmode").action("org_mappings.do_" .. translation[symbol])
	else
		vim.cmd("exe 'normal! " .. vim.v.count1 .. symbol .. "'")
	end
end
vim.keymap.set("n", "<<", function()
	do_demote_promote("<<")
end, {
	buffer = true,
})
vim.keymap.set("n", ">>", function()
	do_demote_promote(">>")
end, {
	buffer = true,
})

-- Map org-mode's <M-CR> behavior into nvim-orgmode
vim.keymap.set("i", "<M-CR>", "<cmd>lua require('orgmode').action('org_mappings.handle_return')<cr>", { buffer = true })

-- Use old todo manager mapping for checkbox toggles:
vim.keymap.set("n", "gtd", "<cmd>lua require('orgmode').action('org_mappings.toggle_checkbox')<cr>", { buffer = true })

-- TODO: This does not correctly update the following:
-- * Foobar [0/1]
-- + Task [0/2]
--    + [ ] Subtask 1
--    + [ ] Subtask 2
-- The headline is not gettting updated. Need to fix that
local Listitem = require("orgmode.treesitter.listitem")
local tree_utils = require("orgmode.utils.treesitter")
local Headline = require("orgmode.treesitter.headline")
local ts_utils = require("nvim-treesitter.ts_utils")
local ts = require("orgmode.treesitter.compat")

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

function Headline:update_cookie(list_node)
	local done = 0
	local total = 0
	local cookie = self:cookie()
	if not cookie then
		return
	end
	if type(list_node) == "userdata" then
		local total_boxes = self:child_checkboxes(list_node)
		local checked_boxes = vim.tbl_filter(function(box)
			return box:match("%[%w%]")
		end, total_boxes)
		total = total + #total_boxes
		done = done + #checked_boxes
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
					local total_boxes = self:child_checkboxes(node:child(0))
					local checked_boxes = vim.tbl_filter(function(box)
						return box:match("%[%w%]")
					end, total_boxes)
					total = total + #total_boxes
					done = done + #checked_boxes
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
end

-- Update cookie statistics:
vim.keymap.set("n", "<leader>o#", function()
	local current_node = tree_utils.get_node_at_cursor()
	local is_ok, listitem = pcall(tree_utils.find_parent_type, current_node, "list")
	if is_ok and type(listitem) == "userdata" and listitem:parent() and listitem:parent():type() == "listitem" then
		listitem = listitem:parent()
	else
		listitem = nil
	end
	local headline = tree_utils.closest_headline()

	local target
	if listitem then
		target = Listitem:new(listitem)
		target:update_checkbox("children")
		return
	end
	if headline then
		target = Headline:new(headline)
		target:update_cookie()
	end
end, { buffer = true })
