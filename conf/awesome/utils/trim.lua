function trim1(s)
   return (s:gsub("^%s*(.-)%s*$", "%1"))
end

return trim1
