function string_literal(str, subs)
	return (str:gsub("$(%w+)", subs))
end

return string_literal
