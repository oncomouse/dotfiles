local function split(text, delimiter)
   local list = {}
   local pos = 1
   if string.find("", delimiter, 1) then -- this would result in endless loops
      error("delimiter matches empty string!")
   end
   while 1 do
      local first, last = string.find(text, delimiter, pos)
      if first then -- found?
         table.insert(list, string.sub(text, pos, first-1))
         pos = last+1
      else
         table.insert(list, string.sub(text, pos))
         break
      end
   end
   return list
end

widget = {
	plugin = "timer",
	opts = {
		period = 1500,
	},
	cb = function()
		local h = io.popen([[curl --connect-timeout 40 -s 'https://wttr.in/?format=1' | sed -e "s/ +//" -e "s/Unknown.*\$//"]])
		local weather = h:read("*a")
		h:close()
		local icon, forecast, pieces
		pieces = split(weather, " ")
		icon = pieces[1]
		forecast = pieces[2]
		return string.format("[^ca(xdg-open http://wttr.in)^fn(JoyPixels:size=9)%s^fn()%s&ca()]", icon, forecast)
	end
}
