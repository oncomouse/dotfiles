-- Source: https://github.com/blitmap/lua-snippets/blob/d02813ef60e956a249ee7dfb8d576981f8ba1cf4/string-trim.lua
local rtrim = function(s)
	local res = s
	local tmp = string.find(res, "%S%s*$")

	if not tmp then
		res = ""
	elseif tmp ~= #res then
		res = string.sub(res, 1, tmp)
	end

	return res, res ~= s
end

return rtrim
