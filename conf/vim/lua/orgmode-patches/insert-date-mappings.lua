local OrgMappings = require("orgmode.org.mappings")
local Calendar = require("orgmode.objects.calendar")
local Date = require("orgmode.objects.date")

function OrgMappings:insertmode_date(inactive)
	local date = self:_get_date_under_cursor()
	if date then
		return Calendar.new({ date = date }).open():next(function(new_date)
			if not new_date then
				return
			end
			self:_replace_date(new_date)
		end)
	end
	vim.cmd("stopinsert!")

	local date_start = self:_get_date_under_cursor(-1)

	return Calendar.new({ date = Date.today() }).open():next(function(new_date)
		if not new_date then
			return
		end
		local date_string = new_date:to_wrapped_string(not inactive)
		if date_start then
			date_string = "--" .. date_string
		end
		vim.cmd(string.format("exe 'norm!i%s' | startinsert!", date_string))
	end)
end

local m = require("orgmode.config.mappings.map_entry")
local mappings = require("orgmode.config.mappings")
mappings.org.org_insertmode_date =
	m.action("org_mappings.insertmode_date", { opts = { desc = "org promote headline" } })
