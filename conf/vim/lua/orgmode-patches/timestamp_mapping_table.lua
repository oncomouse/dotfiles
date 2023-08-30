local OrgMappings = require("orgmode.org.mappings")
local config = require("orgmode.config")

local function table_or_string(value)
    return type(value) == "table" and value[1] or value
end

function OrgMappings:timestamp_up_day()
	return self:_adjust_date(vim.v.count1, "d", vim.v.count1 .. table_or_string(config.mappings.org.org_timestamp_up_day))
end

function OrgMappings:timestamp_down_day()
	return self:_adjust_date(-vim.v.count1, "d", vim.v.count1 .. table_or_string(config.mappings.org.org_timestamp_down_day))
end

function OrgMappings:timestamp_up()
	return self:_adjust_date_part("+", vim.v.count1, vim.v.count1 .. table_or_string(config.mappings.org.org_timestamp_up))
end

function OrgMappings:timestamp_down()
	return self:_adjust_date_part("-", vim.v.count1, vim.v.count1 .. table_or_string(config.mappings.org.org_timestamp_down))
end
