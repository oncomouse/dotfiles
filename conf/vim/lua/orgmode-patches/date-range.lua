-- Just export the stupid class:
local Date = getmetatable(require("orgmode.objects.date").now())
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

