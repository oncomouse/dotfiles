local postfix = require("luasnip.extras.postfix").postfix
-- Infinite lists (based on LaTeX example on LuaSnip's wiki)
local rec_ls
rec_ls = function(delim)
	return function()
		return sn(nil, {
			c(1, {
				-- important!! Having the sn(...) as the first choice will cause infinite recursion.
				t({ "" }),
				-- The same dynamicNode as in the snippet (also note: self reference).
				sn(nil, { t({ "", delim .. " " }), i(1), d(2, rec_ls(delim), {}) }),
			}),
		})
	end
end

return {
	s("cite", fmt("[@{}, {}]", { i(1), i(2) })),
	s("ol", {
		t({ "1. " }),
		i(1),
		d(2, rec_ls("1."), {}),
		i(0),
	}),
	s("ul", {
		t({ "* " }),
		i(1),
		d(2, rec_ls("*"), {}),
		i(0),
	}),
	postfix(".it", {
		l("*" .. l.POSTFIX_MATCH .. "*")
	}),
	postfix(".bd", {
		l("**" .. l.POSTFIX_MATCH .. "**")
	}),
	postfix(".link", {
		d(1, function(_, parent)
			return sn(nil, {
				t("[" .. parent.env.POSTFIX_MATCH .. "]"),
				t("("),
				i(1),
				t(")"),
			})
		end)
	})
}
