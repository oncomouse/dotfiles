-- luacheck: globals vim
local get_wal_theme = require('dotfiles.utils.wal')
local json_scheme = get_wal_theme()
local wal = {
	color0 = { c = 0, gui = json_scheme.colors.color0 and json_scheme.colors.color0 or "#000000" },
	color1 = { c = 1, gui = json_scheme.colors.color1 and json_scheme.colors.color1 or "#FF0000" },
	color2 = { c = 2, gui = json_scheme.colors.color2 and json_scheme.colors.color2 or "#00FF00" },
	color3 = { c = 3, gui = json_scheme.colors.color3 and json_scheme.colors.color3 or "#FFFF00" },
	color4 = { c = 4, gui = json_scheme.colors.color4 and json_scheme.colors.color4 or "#0000FF" },
	color5 = { c = 5, gui = json_scheme.colors.color5 and json_scheme.colors.color5 or "#FF00FF" },
	color6 = { c = 6, gui = json_scheme.colors.color6 and json_scheme.colors.color6 or "#00FFFF" },
	color7 = { c = 7, gui = json_scheme.colors.color7 and json_scheme.colors.color7 or "#000000" },
	color8 = { c = 8, gui = json_scheme.colors.color8 and json_scheme.colors.color8 or "#FFFFFF" },
	color9 = { c = 9, gui = json_scheme.colors.color9 and json_scheme.colors.color9 or "#FF0000" },
	color10 = { c = 10, gui = json_scheme.colors.color10 and json_scheme.colors.color10 or "#00FF00" },
	color11 = { c = 11, gui = json_scheme.colors.color11 and json_scheme.colors.color11 or "#FFFF00" },
	color12 = { c = 12, gui = json_scheme.colors.color12 and json_scheme.colors.color12 or "#0000FF" },
	color13 = { c = 13, gui = json_scheme.colors.color13 and json_scheme.colors.color13 or "#FF00FF" },
	color14 = { c = 14, gui = json_scheme.colors.color14 and json_scheme.colors.color14 or "#00FFFF" },
	color15 = { c = 15, gui = json_scheme.colors.color15 and json_scheme.colors.color15 or "#FFFFFF" },
	-- Base16 Colors are optional, so we guess for them:
	base00 = { c = 235, gui = json_scheme.colors.base00 and json_scheme.colors.base00 or json_scheme.colors.color0 },
	base01 = { c = 237, gui = json_scheme.colors.base01 and json_scheme.colors.base01 or json_scheme.colors.color8 },
	base02 = { c = 240, gui = json_scheme.colors.base02 and json_scheme.colors.base02 or "#585858" },
	base03 = { c = 243, gui = json_scheme.colors.base03 and json_scheme.colors.base03 or "#767676" },
	base04 = { c = 145, gui = json_scheme.colors.base04 and json_scheme.colors.base04 or json_scheme.colors.color7 },
	base05 = { c = 251, gui = json_scheme.colors.base05 and json_scheme.colors.base05 or json_scheme.colors.color15 },
	base06 = { c = 252, gui = json_scheme.colors.base06 and json_scheme.colors.base06 or "#d0d0d0" },
	base07 = { c = 253, gui = json_scheme.colors.base07 and json_scheme.colors.base07 or "#dadada" },
	red = { c = 5, gui = json_scheme.colors.base08 and json_scheme.colors.red or json_scheme.colors.color5 },
	orange = { c = 1, gui = json_scheme.colors.base09 and json_scheme.colors.base09 or json_scheme.colors.color1 },
	yellow = { c = 3, gui = json_scheme.colors.base0A and json_scheme.colors.base0A or json_scheme.colors.color3 },
	green = { c = 2, gui = json_scheme.colors.base0B and json_scheme.colors.base0B or json_scheme.colors.color2 },
	cyan = { c = 6, gui = json_scheme.colors.base0C and json_scheme.colors.base0C or json_scheme.colors.color6 },
	blue = { c = 4, gui = json_scheme.colors.base0D and json_scheme.colors.base0D or json_scheme.colors.color4 },
	purple = { c = 176, gui = json_scheme.colors.base0E and json_scheme.colors.base0E or '#D4BFFF' },
	brown = { c = 9, gui = json_scheme.colors.base0F and json_scheme.colors.base0F or json_scheme.colors.color9 },
}

function wal.terminal_colors()
	vim.g.terminal_color_0 = wal.color0.gui
	vim.g.terminal_color_1 = wal.color1.gui
	vim.g.terminal_color_2 = wal.color2.gui
	vim.g.terminal_color_3 = wal.color3.gui
	vim.g.terminal_color_4 = wal.color4.gui
	vim.g.terminal_color_5 = wal.color5.gui
	vim.g.terminal_color_6 = wal.color6.gui
	vim.g.terminal_color_7 = wal.color7.gui
	vim.g.terminal_color_8 = wal.color8.gui
	vim.g.terminal_color_9 = wal.color9.gui
	vim.g.terminal_color_10 = wal.color10.gui
	vim.g.terminal_color_11 = wal.color11.gui
	vim.g.terminal_color_12 = wal.color12.gui
	vim.g.terminal_color_13 = wal.color13.gui
	vim.g.terminal_color_14 = wal.color14.gui
	vim.g.terminal_color_15 = wal.color15.gui
end
function wal.highlight(group, color)
	if color.link then
		vim.api.nvim_command('highlight default link ' .. group .. ' ' .. color.link)
		return
	end
	local style = color.style and 'gui=' .. color.style or 'gui=NONE'
	local term_style = color.term and ' term=' .. color.term or 'term=NONE'
	local fg = color.fg and 'guifg=' .. color.fg.gui .. ' ctermfg=' .. color.fg.c or 'guifg=NONE ctermfg=NONE'
	local bg = color.bg and 'guibg=' .. color.bg.gui .. ' ctermbg=' .. color.bg.c or 'guibg=NONE ctermbg=NONE'
	local sp = color.sp and 'guisp=' .. color.sp.gui or ''
   vim.api.nvim_command(
		'highlight ' .. group .. ' ' ..
		style .. ' ' ..
		term_style .. ' ' ..
		fg .. ' ' ..
		bg .. ' ' ..
		sp
	)
end
function wal.after_scheme()
	local scheme = {
		LspDiagnosticsDefaultError = { },
		LspDiagnosticsSignError = { fg=wal.red },
		LspDiagnosticsUnderlineError = { style="undercurl" },

		LspDiagnosticsDefaultWarning = { },
		LspDiagnosticsSignWarning = { fg=wal.yellow },
		LspDiagnosticsUnderlineWarning = { style="undercurl" },

		LspDiagnosticsDefaultInformation = { },
		LspDiagnosticsSignInformation = { fg=wal.blue },
		LspDiagnosticsUnderlineInformation = { style="undercurl" },

		LspDiagnosticsDefaultHint = { },
		LspDiagnosticsSignHint = { fg=wal.cyan },
		LspDiagnosticsUnderlineHint = { style="undercurl" },

		TSAnnotation = { fg = wal.blue },
		TSAttribute = { fg = wal.blue },
		TSBoolean = { fg = wal.blue },
		TSCharacter = { fg = wal.yellow },
		TSComment = { fg = wal.base01 },
		TSConditional = { fg = wal.red },
		TSConstBuiltin = { fg = wal.orange },
		TSConstMacro = { fg = wal.orange },
		TSConstant = { fg = wal.orange },
		TSConstructor = { fg = wal.base05 },
		TSEmphasis = { style="bold" },
		TSError = { fg = wal.red },
		TSException = { fg = wal.red },
		TSField = { fg = wal.green },
		TSFloat = { fg = wal.blue },
		TSFuncBuiltin = { fg = wal.orange },
		TSFuncMacro = { fg = wal.orange },
		TSFunction = { fg = wal.cyan },
		TSInclude = { fg = wal.cyan },
		TSKeyword = { fg = wal.purple },
		TSKeywordFunction = { fg = wal.cyan },
		TSKeywordOperator = { fg = wal.purple },
		TSLabel = { fg = wal.cyan },
		TSMethod = { fg = wal.blue },
		TSNamespace = { fg = wal.blue },
		TSNumber = { fg = wal.blue },
		TSOperator = { fg = wal.base05 },
		TSParameter = { fg = wal.yellow },
		TSParameterReference = { fg = wal.orange },
		TSProperty = { fg = wal.yellow },
		TSPunctBracket = { fg = wal.cyan },
		TSPunctDelimiter = { fg = wal.base05 },
		TSPunctSpecial = { fg = wal.base05 },
		TSRepeat = { fg = wal.red },
		TSString = { fg = wal.blue },
		TSStringEscape = { fg = wal.green },
		TSStringRegex = { fg = wal.green },
		TSStructure = { fg = wal.blue },
		TSTag = { fg = wal.yellow },
		TSTagDelimiter = { fg = wal.cyan },
		TSText = { fg = wal.green },
		TSType = { fg = wal.blue },
		TSTypeBuiltin = { fg = wal.blue },
		TSURI = { style="underline", bg=wal.base01 },
		TSUnderline = {style="underline"},
		TSVariable = { fg = wal.yellow },
		TSVariableBuiltin = { fg = wal.red },

		SpellBad = { style="underline", bg=wal.base01, sp=wal.red },
		SpellLocal = { style="underline", bg=wal.base01, sp=wal.cyan },
		SpellCap = { style="underline", bg=wal.base01, sp=wal.yellow },
		SpellRare = { style="underline", bg=wal.base01, sp=wal.purple },

		csClass = { fg=wal.yellow },
		csAttribute = { fg=wal.yellow },
		csModifier = { fg=wal.purple },
		csType = { fg=wal.red },
		csUnspecifiedStatement = { fg=wal.blue },
		csContextualStatement = { fg=wal.purple },
		csNewDecleration = { fg=wal.red },
		cOperator = { fg=wal.cyan },
		cPreCondit = { fg=wal.purple },

		cssColor = { fg=wal.cyan },
		cssBraces = { fg=wal.base05 },
		cssClassName = { fg=wal.purple },

		DiffAdd = { fg=wal.green, bg=wal.base01, style="bold" },
		DiffChange = { fg=wal.base03, bg=wal.base01 },
		DiffDelete = { fg=wal.red, bg=wal.base01 },
		DiffText = { fg=wal.blue, bg=wal.base01 },
		DiffAdded = { fg=wal.base05, bg=wal.green, style="bold" },
		DiffFile = { fg=wal.red, bg=wal.base00 },
		DiffNewFile = { fg=wal.green, bg=wal.base00 },
		DiffLine = { fg=wal.blue, bg=wal.base00 },
		DiffRemoved = { fg=wal.base05, bg=wal.red, style="bold" },

		gitCommitOverflow = { fg=wal.red },
		gitCommitSummary = { fg=wal.green },

		htmlBold = { fg=wal.yellow },
		htmlItalic = { fg=wal.purple },
		htmlTag = { fg=wal.cyan },
		htmlEndTag = { fg=wal.cyan },
		htmlArg = { fg=wal.yellow },
		htmlTagName = { fg=wal.base05 },

		javaScript = { fg=wal.base05 },
		javaScriptNumber = { fg=wal.orange },
		javaScriptBraces = { fg=wal.base05 },

		jsonKeyword = { fg=wal.green },
		jsonQuote = { fg=wal.green },

		markdownCode = { fg=wal.green },
		markdownCodeBlock = { fg=wal.green },
		markdownHeadingDelimiter = { fg=wal.blue },
		markdownItalic = { fg=wal.purple, style="italic" },
		markdownBold = { fg=wal.yellow, style="bold" },
		markdownCodeDelimiter = { fg=wal.brown, style="italic" },
		markdownError = { fg=wal.base05, bg=wal.base00 },

		typescriptParens = { fg=wal.base05, bg=wal.none },

		NeomakeErrorSign = { fg=wal.red, bg=wal.base00 },
		NeomakeWarningSign = { fg=wal.yellow, bg=wal.base00 },
		NeomakeInfoSign = { fg=wal.white, bg=wal.base00 },
		NeomakeError = { fg=wal.red, style='underline', sp=wal.red },
		NeomakeWarning = { fg=wal.red, style='underline', sp=wal.red },
		ALEErrorSign = { fg=wal.red, bg=wal.base00, style="bold" },
		ALEWarningSign = { fg=wal.yellow, bg=wal.base00, style="bold" },
		ALEInfoSign = { fg=wal.white, bg=wal.base00, style="bold" },

		NERDTreeExecFile = { fg=wal.base05 },
		NERDTreeDirSlash = { fg=wal.blue },
		NERDTreeOpenable = { fg=wal.blue },
		NERDTreeFile = { fg=wal.none },
		NERDTreeFlags = { fg=wal.blue },

		phpComparison = { fg=wal.base05 },
		phpParent = { fg=wal.base05 },
		phpMemberSelector = { fg=wal.base05 },

		pythonRepeat = { fg=wal.purple },
		pythonOperator = { fg=wal.purple },

		rubyConstant = { fg=wal.yellow },
		rubySymbol = { fg=wal.green },
		rubyAttribute = { fg=wal.blue },
		rubyInterpolation = { fg=wal.green },
		rubyInterpolationDelimiter = { fg=wal.brown },
		rubyStringDelimiter = { fg=wal.green },
		rubyRegexp = { fg=wal.cyan },

		sassidChar = { fg=wal.red },
		sassClassChar = { fg=wal.orange },
		sassInclude = { fg=wal.purple },
		sassMixing = { fg=wal.purple },
		sassMixinName = { fg=wal.blue },

		vimfilerLeaf = { fg=wal.base05 },
		vimfilerNormalFile = { fg=wal.base05, bg=wal.base00 },
		vimfilerOpenedFile = { fg=wal.blue },
		vimfilerClosedFile = { fg=wal.blue },

		GitGutterAdd = { fg=wal.green, bg=wal.base00, style="bold" },
		GitGutterChange = { fg=wal.blue, bg=wal.base00, style="bold" },
		GitGutterDelete = { fg=wal.red, bg=wal.base00, style="bold" },
		GitGutterChangeDelete = { fg=wal.purple, bg=wal.base00, style="bold" },

		SignifySignAdd = { fg=wal.green, bg=wal.base00, style="bold" },
		SignifySignChange = { fg=wal.blue, bg=wal.base00, style="bold" },
		SignifySignDelete = { fg=wal.red, bg=wal.base00, style="bold" },
		SignifySignChangeDelete = { fg=wal.purple, bg=wal.base00, style="bold" },
		SignifySignDeleteFirstLine = { fg=wal.red, bg=wal.base00, style="bold" },

		xmlTag = { fg=wal.cyan },
		xmlTagName = { fg=wal.base05 },
		xmlEndTag = { fg=wal.cyan },
		Defx_filename_directory = { fg=wal.blue },

		TelescopeBorder = { fg=wal.blue },
		TelescopePromptBorder = { fg=wal.cyan },

		CocErrorSign = { fg=wal.red },
		CocWarningSign = { fg=wal.yellow },
		CocInfoSign = { fg=wal.blue },
		CocHintSign = { fg=wal.cyan },
		CocErrorFloat = { fg=wal.red },
		CocWarningFloat = { fg=wal.yellow },
		CocInfoFloat = { fg=wal.blue },
		CocHintFloat = { fg=wal.cyan },
		CocDiagnosticsError = { fg=wal.red },
		CocDiagnosticsWarning = { fg=wal.yellow },
		CocDiagnosticsInfo = { fg=wal.blue },
		CocDiagnosticsHint = { fg=wal.cyan },
		CocSelectedText = { fg=wal.purple },
		CocCodeLens = { fg=wal.base04 },
		semshiLocal = { fg=wal.brown},
		semshiGlobal = { fg=wal.orange},
		semshiImported = { fg=wal.brown, style="bold"},
		semshiParameter = { fg=wal.blue},
		semshiParameterUnused = {fg=wal.color12, style="underline"},
		semshiFree = { fg=wal.red},
		semshiBuiltin = { fg=wal.color13},
		semshiAttribute = { fg=wal.cyan},
		semshiSelf = { fg=wal.base04},
		semshiUnresolved = { fg=wal.yellow, style="underline"},
		semshiSelected = { fg=wal.background, bg={ gui = "#ff875f", c=209 }},
		semshiErrorSign = { fg=wal.foreground, bg={ gui = "#d70000", c=160 }},
		semshiErrorChar = { fg=wal.foreground, bg={ gui = "#d70000", c=160 }},
		lualine_z_diagnostics_error_command = { fg=wal.base01, bg=wal.red },
		lualine_z_diagnostics_error_insert = { fg=wal.base01, bg=wal.red },
		lualine_z_diagnostics_error_normal = { fg=wal.base01, bg=wal.red },
		lualine_z_diagnostics_error_replace = { fg=wal.base01, bg=wal.red },
		lualine_z_diagnostics_error_terminal = { fg=wal.base01, bg=wal.red },
		lualine_z_diagnostics_error_visual = { fg=wal.base01, bg=wal.red },
		lualine_z_diagnostics_warn_command = { fg=wal.base01, bg=wal.orange },
		lualine_z_diagnostics_warn_insert = { fg=wal.base01, bg=wal.orange },
		lualine_z_diagnostics_warn_normal = { fg=wal.base01, bg=wal.orange },
		lualine_z_diagnostics_warn_replace = { fg=wal.base01, bg=wal.orange },
		lualine_z_diagnostics_warn_terminal = { fg=wal.base01, bg=wal.orange },
		lualine_z_diagnostics_warn_visual = { fg=wal.base01, bg=wal.orange },
		lualine_z_diagnostics_info_command = { fg=wal.base01, bg=wal.cyan },
		lualine_z_diagnostics_info_insert = { fg=wal.base01, bg=wal.cyan },
		lualine_z_diagnostics_info_normal = { fg=wal.base01, bg=wal.cyan },
		lualine_z_diagnostics_info_replace = { fg=wal.base01, bg=wal.cyan },
		lualine_z_diagnostics_info_terminal = { fg=wal.base01, bg=wal.cyan },
		lualine_z_diagnostics_info_visual = { fg=wal.base01, bg=wal.cyan },
	}
	return scheme
end
function wal.scheme()
	local scheme = {
		Bold = { style="bold" },
		Debug = { fg=wal.red },
		Directory = { fg=wal.blue },
		ErrorMsg = { fg=wal.red, bg=wal.base00 },
		Exception = { fg=wal.red },
		FoldColumn = { fg=wal.blue, bg=wal.base00 },
		Folded = { fg=wal.base04, bg=wal.base01, style="italic" },
		IncSearch = { fg=wal.base01, bg=wal.orange, style="NONE" },
		Italic = { style="italic" },

		Macro = { fg=wal.red },
		MatchParen = { fg=wal.base05, bg=wal.base03 },
		ModeMsg = { fg=wal.green },
		MoreMsg = { fg=wal.green },
		Question = { fg=wal.blue },
		Search = { fg=wal.base03, bg=wal.yellow },
		SpecialKey = { fg=wal.base03 },
		TooLong = { fg=wal.red },
		Underlined = { fg=wal.red },
		Visual = { bg=wal.base02, fg=wal.base00 },
		VisualNOS = { fg=wal.red },
		WarningMsg = { fg=wal.red },
		WildMenu = { fg=wal.base05, bg=wal.blue },
		Title = { fg=wal.blue },
		Conceal = { fg=wal.base03, bg=wal.base00 },
		Cursor = { fg=wal.base00, bg=wal.base05 },
		NonText = { fg=wal.base03 },
		Normal = { fg=wal.base05, bg=wal.base00 },
		EndOfBuffer = { fg=wal.base05, bg=wal.base00 },
		LineNr = { fg=wal.base04, bg=wal.base00 },
		SignColumn = { fg=wal.base02, bg=wal.base00 },
		StatusLine = { fg=wal.base01, bg=wal.base03 },
		StatusLineNC = { fg=wal.base03, bg=wal.base01 },
		VertSplit = { fg=wal.base00, bg=wal.base02 },
		ColorColumn = { fg=wal.base02 },
		CursorColumn = { fg=wal.base02 },
		CursorLine = { fg=wal.base01, style="None" },
		CursorLineNR = { fg=wal.base00, bg=wal.base00 },
		CursorLineNr = { fg=wal.base03, bg=wal.base01 },
		PMenu = { fg=wal.base04, bg=wal.base01 },
		PMenuSel = { fg=wal.base05, bg=wal.blue },
		PmenuSbar = { fg=wal.base02 },
		PmenuThumb = { fg=wal.base05 },
		TabLine = { fg=wal.base03, bg=wal.base01 },
		TabLineFill = { fg=wal.base03, bg=wal.base01 },
		TabLineSel = { fg=wal.green, bg=wal.base01 },
		helpExample = { fg=wal.yellow },
		helpCommand = { fg=wal.yellow },

		Boolean = { fg=wal.orange },
		Character = { fg=wal.red },
		Comment = { fg=wal.base04, style="italic" },
		Conditional = { fg=wal.purple },
		Constant = { fg=wal.orange },
		Define = { fg=wal.purple },
		Delimiter = { fg=wal.brown },
		Float = { fg=wal.orange },
		Function = { fg=wal.blue },

		Identifier = { fg=wal.cyan },
		Include = { fg=wal.blue },
		Keyword = { fg=wal.purple },

		Label = { fg=wal.yellow },
		Number = { fg=wal.orange },
		Operator = { fg=wal.base05 },
		PreProc = { fg=wal.yellow },
		Repeat = { fg=wal.yellow },
		Special = { fg=wal.cyan },
		SpecialChar = { fg=wal.brown },
		Statement = { fg=wal.red },
		StorageClass = { fg=wal.yellow },
		String = { fg=wal.green },
		Structure = { fg=wal.purple },
		Tag = { fg=wal.yellow },
		Todo = { fg=wal.yellow, bg=wal.base01 },
		Type = { fg=wal.yellow },
		Typedef = { fg=wal.yellow },

	}
	return scheme
end
local async_load_plugin
async_load_plugin = vim.loop.new_async(vim.schedule_wrap(function ()
	wal.terminal_colors()
	local syntax = wal.after_scheme()
	for group,colors in pairs(syntax) do
		wal.highlight(group,colors)
	end
	async_load_plugin:close()
end))
function wal.colorscheme()
	vim.cmd('hi clear')
	if vim.fn.exists('syntax_on') then
		vim.cmd('syntax reset')
	end
	vim.o.background = 'dark'
	vim.o.termguicolors = true
	vim.g.colors_name = 'wal'
	local syntax = wal.scheme()
	for group,colors in pairs(syntax) do
		wal.highlight(group,colors)
	end
	async_load_plugin:send()
end

wal.colorscheme()

return wal
