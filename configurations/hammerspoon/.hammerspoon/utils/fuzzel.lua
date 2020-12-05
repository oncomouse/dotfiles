--[[
	Fuzzel v1.4 - Alexander "Apickx" Pickering
	Entered into the public domain June 2, 2016
	You are not required to, but consider putting a link to the source in your file's comments!

	Example:
		Returns a function that will return the closest string to the string it was passed
		-----------------FuzzelExample.lua------------------
		--Include the module
		local fuzzel = require("fuzzel.lua")

		--A couple of options
		local options = {
			"Fat Cat",
			"Lazy Dog",
			"Brown Fox",
		}

		--And use it, to see what option closest matches "Lulzy Cat"
		local close,distance = fuzzel.FuzzyFindDistance("Lulzy Cat", options)
		print("\"Lulzy Cat\" is close to \"" .. close .. "\", distance:" .. distance)

		--Sort the options to see the order in which they most closely match "Frag God"
		print("\"Frag God\" is closest to:")
		for k,v in ipairs(fuzzel.FuzzySortRatio("Frag God",options)) do
			print(k .. "\t:\t" .. v)
		end
		-------------End FuzzelExample.lua------------------
		Outputs:
			"Lulzy Cat" is close to "Fat Cat"
			"Frag God" is closest to:
			1       :       Fat Cat
			2       :       Lazy Dog
			3       :       Brown Fox

	Some easy-to-use mnemonics
		fuzzel.ld_e = fuzzel.LevenshteinDistance_extended
		fuzzel.ld = fuzzel.LevenshteinDistance
		fuzzel.lr = fuzzel.LevensteinRatio
		fuzzel.dld_e = fuzzel.DamerauLevenshteinDistance_extended
		fuzzel.dld = fuzzel.DamerauLevenshteinDistance
		fuzzel.dlr = fuzzel.DamerauLevenshteinRatio
		fuzzel.hd = fuzzel.HammingDistance
		fuzzel.hr = fuzzel.HammingRatio
		fuzzel.ffd = fuzzel.FuzzyFindDistance
		fuzzel.ffr = fuzzel.FuzzyFindRatio
		fuzzel.fsd = fuzzel.FuzzySortDistance
		fuzzel.fsr = fuzzel.FuzzySortRatio
		fuzzel.fad = fuzzel.FuzzyAutocompleteDistance
		fuzzel.far = fuzzel.FuzzyAutocompleteRatio

]] --You probably don't want to touch anything past this point

--- A collection of methods for finding edit distance between two strings

-- local utf8 = require"lua-utf8"
--Assign locals to these to the minifier can compress the file better
local strlen,
	chrat,
	min,
	asrt,
	prs,
	iprs,
	typ,
	upack,
	tblins,
	tblsrt,
	strsub,
	tru,
	fal,
	tblcat
=
	utf8.len,
	string.byte,
	math.min,
	assert,
	pairs,
	ipairs,
	type,
	unpack,
	table.insert,
	table.sort,
	string.sub,
	true,
	false,
	table.concat

local fuzzel = {}

--A clever way to allow the minifier to minify function names, this basically just assigns variables with their string equivalent.
local da, le, di, ra, fu, fi, so, ex, ha, au =
	"Damerau",
	"Levenshtein",
	"Distance",
	"Ratio",
	"Fuzzy",
	"Find",
	"Sort",
	"_extended",
	"Hamming",
	"Autocomplete"
local LevenshteinDistance_extended,
	LevenshteinDistance,
	LevenshteinRatio,
	DamerauLevenshteinDistance_extended,
	DamerauLevenshteinDistance,
	DamerauLevenshteinRatio,
	FuzzyFindDistance,
	FuzzyFindRatio,
	FuzzySortDistance,
	FuzzySortRatio,
	HammingDistance,
	HammingRatio,
	FuzzyAutocompleteDistance,
	FuzzyAutocompleteRatio,
	Table
=
	le .. di .. ex,
	le .. di,
	le .. ra,
	da .. le .. di .. ex,
	da .. le .. di,
	da .. le .. ra,
	fu .. fi .. di,
	fu .. fi .. ra,
	fu .. so .. di,
	fu .. so .. ra,
	ha .. di,
	ha .. ra,
	fu .. au .. di,
	fu .. au .. ra,
	"table"

local function genericDistance(stringa, stringb, addcost, subcost, delcost, ...)
	local arg = { ... }

	--Length of each string
	local salen, sblen = strlen(stringa), strlen(stringb)

	--Create a 0 matrix the size of len(a) x len(b)
	local dyntbl = {}
	for i = 0, salen do
		dyntbl[i] = {}
		for j = 0, sblen do
			dyntbl[i][j] = 0
hs.inspect.inspect(		)end
	end

	--Initalize the matrix
	for i = 1, salen do
		dyntbl[i][0] = i
	end
	for j = 1, sblen do
		dyntbl[0][j] = j
	end

	--And build up the matrix based on costs-so-far
	for j = 1, sblen do
		for i = 1, salen do
			local ca, cb = chrat(stringa, i), chrat(stringb, j)
			dyntbl[i][j] = min(
				dyntbl[i - 1][j] + delcost, --deletion
				dyntbl[i][j - 1] + addcost, --insertion
				dyntbl[i - 1][j - 1] + (ca == cb and 0 or subcost) --substituion
			)
			if arg[1] and i > 1 and j > 1 and ca == chrat(
				stringb,
				j - 1
			) and chrat(stringa, i - 1) == cb then
				dyntbl[i][j] =
					min(
						dyntbl[i][j],
						dyntbl[i - 2][j - 2] + (ca == cb and 0 or arg[2])
					) --transposition
			end
		end
	end

	return dyntbl[salen][sblen]
end

--- The current version (1.4).
fuzzel._VERSION = "1.4"

--- Finds edit distance between two strings with custom costs.
-- The levenshtein distance is the minimum number of additions, deletions, and substitutions that are needed to turn one string into another. This methods allows custom costs for addition, deletion, and substitution.
-- @function LevenshteinDistance_extended
-- @string str1 the first string
-- @string str2 the second string
-- @number addcost the custom cost to add one character
-- @number subcost the custom cost to subtitute one character for another
-- @number delcost the custom cost to delete one character
-- @return the distance from the first string to the second (which will always  be the same as the distance from the second string to the first)
-- @usage fuzzel.LevenshteinDistance_extended("juice","moose",1,2,3)
fuzzel[LevenshteinDistance_extended] = function(
stringa,
	stringb,
	addcost,
	subcost,
	delcost
)
	return genericDistance(stringa, stringb, addcost, subcost, delcost)
end
fuzzel.ld_e = fuzzel[LevenshteinDistance_extended]

--- Finds simple Levenshtein distance.
-- The levenshtein distance is the minimum number of additions, deletions, and substitutions that are needed to turn one string into another.
-- @function LevenshteinDistance
-- @string str1 the first string
-- @string str2 the second string
-- @return the distance between the two input strings
-- @usage fuzzel.LevenshteinDistance("Flag","Brag")
fuzzel[LevenshteinDistance] = function(stringa, stringb)
	return fuzzel.ld_e(stringa, stringb, 1, 1, 1)
end
fuzzel.ld = fuzzel[LevenshteinDistance]

--- Finds edit ratio between two strings
-- @function LevenshteinRatio
-- @string str1 the first string, and the string to use for the ratio
-- @string str2 the second string
-- @return the distance between the two strings divided by the length of the first string
-- @usage fuzzel.LevenshteinRatio("bling","bring")
fuzzel[LevenshteinRatio] = function(stringa, stringb)
	return fuzzel.ld(stringa, stringb) / strlen(stringa)
end
fuzzel.lr = fuzzel[LevenshteinRatio]

--- Finds edit distance between two strings, with custom values.
-- The minimum number of additions, deletions, substitutions, or transpositions to turn str1 into str2 with the given weights
-- @function DamerauLevenshteinDistance_extended
-- @string str1 the first string
-- @string str2 the second string
-- @number addcost the cost of insterting a character
-- @number subcost the cost of substituteing one character for another
-- @number delcost the cost of removeing a character
-- @number trncost the cost of transposeing two adjacent characters.
-- @return the edit distance between the two strings
-- @usage DamerauLevenshteinDistance_extended("berry","bury",0,1,1,2,1)
fuzzel[DamerauLevenshteinDistance_extended] = function(
stringa,
	stringb,
	addcost,
	subcost,
	delcost,
	trncost
)
	return genericDistance(
		stringa,
		stringb,
		addcost,
		subcost,
		delcost,
		tru,
		trncost
	)
end
fuzzel.dld_e = fuzzel[DamerauLevenshteinDistance_extended]

--- Finds simple Damerau-Levenshtein distance.
-- @function DamerauLevenshteinDistance
-- @string str1 the fist string
-- @string str2 the second string
-- @return the minimum number of additions, deletions, substitutions, or transpositions to turn str1 into str2
-- @usage fuzzel.DamerauLevenshteinDistance("tree","trap")
fuzzel[DamerauLevenshteinDistance] = function(stringa, stringb)
	return fuzzel.dld_e(stringa, stringb, 1, 1, 1, 1)
end
fuzzel.dld = fuzzel[DamerauLevenshteinDistance]

--- Finds edit ratio between two strings
-- @function DamerauLevenshteinRatio
-- @string str1 the fist string, and number to use for ratio
-- @string str2 the second string
-- @return the Damerau-Levenshtein distance divided by the length of the first string.
-- @usage fuzzel.DamerauLevenshteinRatio("pants","hands")
fuzzel[DamerauLevenshteinRatio] = function(stringa, stringb)
	return fuzzel.dld(stringa, stringb) / strlen(stringa)
end
fuzzel.dlr = fuzzel[DamerauLevenshteinRatio]

--- Finds the nubmer of subtitutions needed to turn one string into another.
-- Hamming distance can only be calculated on two strings of equal length.
-- @function HammingDistance
-- @string str1 the first string
-- @string str2 the second string
-- @return the edit distance between str1 and str2
-- @usage fuzzel.HammingDistance("one","two")
-- @usage fuzzel.HammingDistance("two","three") --Will throw an error, since "two" is 3 characters long while "three" is 5 characters long!
fuzzel[HammingDistance] = function(stringa, stringb)
	local len, dist = strlen(stringa), 0
	asrt(
		len == strlen(stringb),
		tblcat(
			{
				ha,
				" ",
				di,
				' cannot be calculated on two strings of different lengths:"',
				stringa,
				'" "',
				stringb,
				'"',
			}
		)
	)
	for i = 1, len do
		dist = dist + ((chrat(stringa, i) ~= chrat(stringb, i)) and 1 or 0)
	end
	return dist
end
fuzzel.hd = fuzzel[HammingDistance]

--- Calculates the Hamming distance between two strings, divided by the length of the first string.
-- @function HammingRatio
-- @string str1 the first string, and string to use for the ratio
-- @string str2 the second string
-- @return the edit distance between the two strings
-- @usage fuzzel.HammingRatio("four","five")
-- @usage fuzzel.HammingRatio("seven","ten") -- Will throw an error, since "seven" is 5 characters long while "ten" is 3 characters long
fuzzel[HammingRatio] = function(stringa, stringb)
	return fuzzel.hd(stringa, stringb) / strlen(stringa)
end
fuzzel.hr = fuzzel[HammingRatio]

local function FuzzySearch(str, func, ...)
	local arg = { ... }

	--Allow varargs, or a table
	local looparg = typ(arg[1]) == Table and arg[1] or arg

	--Find the string with the shortest distance to the string we were supplied
	local tmin, sout = func(looparg[1], str), looparg[1]
	for k, v in prs(looparg) do
		local t = func(v, str)
		if t <= tmin then
			tmin, sout = t, k
		end
	end
	return looparg[sout], tmin
end

--- Finds the closest argument to the first argument.
-- Finds the closest argument to the first argument useing Damerau-Levenshtein distance
-- @function FuzzyFindDistance
-- @string str the string to compare to
-- @param ... A 1-indexed array of strings, or a list of strings to campare str against
-- @usage fuzzel.FuzzyFindDistance("tap","tape","strap","tab")
-- @usage fuzzel.FuzzyFindDistance("tap",{"tape","strap","tab"})
fuzzel[FuzzyFindDistance] = function(str, ...)
	return upack{ FuzzySearch(str, fuzzel.dld, ...) }
end
fuzzel.ffd = fuzzel[FuzzyFindDistance]

--- Finds the closest argument to the first argument.
-- Finds the closest argument to the first argument useing Damerau-Levenshtein ratio
-- @function FuzzyFindRatio
-- @string str the string to compare to
-- @param ... A 1-indexed array of strings, or a list of strings to campare str against
-- @usage fuzzel.FuzzyFindRatio("light",{"lit","lot","lightbulb"})
-- @usage fuzzel.FuzzyFindRatio("light","lit","lot","lightbulb")
fuzzel[FuzzyFindRatio] = function(str, ...)
	return upack{ FuzzySearch(str, fuzzel.dlr, ...) }
end
fuzzel.ffr = fuzzel[FuzzyFindRatio]

local function FuzzySort(str, func, short, ...)
	local arg = { ... }

	--allow varargs, or a table
	local looparg = typ(arg[1]) == Table and arg[1] or arg

	--Roughly sort everything by it's distance to the string
	local usorted, sorted, otbl, slen = {}, {}, {}, strlen(str)
	for k, v in prs(looparg) do
		local sstr = short and strsub(v, 0, slen) or v
		local dist = func(str, sstr)
		if usorted[dist] == nil then
			usorted[dist] = {}
			tblins(sorted, dist)
		end
		tblins(usorted[dist], v)
	end

	--Actually sort them into something can can be iterated with ipairs
	tblsrt(sorted)

	--Then build our output table
	for k, v in iprs(sorted) do
		for i, j in prs(usorted[v]) do
			tblins(otbl, j)
		end
	end
	return otbl
end

--- Sorts inputed strings by distance.
-- Finds the Damerau-Levenshtein distance of each string to the first argument, and sorts them into a table accordingly
-- @function FuzzySortDistance
-- @string str the string to compare each result to
-- @param ... either a 1-indexed table, or a list of strings to sort
-- @return a 1-indexed table of the input strings, in the order of closest-to str to farthest-from str
-- @usage fuzzel.FuzzySortDistance("tub",{"toothpaste","stub","tube"})
-- @usage fuzzel.FuzzySortDistance("tub","toothpaste","stub","tube")
fuzzel[FuzzySortDistance] = function(str, ...)
	return FuzzySort(str, fuzzel.dld, fal, ...)
end
fuzzel.fsd = fuzzel[FuzzySortDistance]

--- Sorts inputed strings by ratio.
-- Finds the Damerau-Levenshtein ratio of each string to the first argument, and sorts them into a table accordingly
-- @function FuzzySortRatio
-- @string str the string to compare each result to
-- @param ... either a 1-indexed table, or a list of strings to sort
-- @return a 1-indexed table of the input strings, in the order of closest-to str to farthest-from str
-- @usage fuzzel.FuzzySortRatio("can",{"candle","candie","canister"})
-- @usage fuzzel.FuzzySortRatio("can","candle","candie","canister")
fuzzel[FuzzySortRatio] = function(str, ...)
	return FuzzySort(str, fuzzel.dlr, fal, ...)
end
fuzzel.fsr = fuzzel[FuzzySortRatio]

--- Sorts truncated versions of inputed strings by distance.
-- truncates each input string, and finds the Damerau-Levenshtein distance of each string to the first argument, and sorts them into a table accordingly. Useful for auto-complete functions
-- @function FuzzyAutocompleteDistance
-- @string str the string to compare each result to
-- @param ... either a 1-indexed table, or a list of strings to sort
-- @return a 1-indexed table of the input strings, in the order of closest-to str to farthest-from str
-- @usage fuzzel.FuzzyAutocompleteDistance("brow",{"brown","brownie","high-brow"})
-- @usage fuzzel.FuzzyAutocompleteDistance("brow","brown","brownie","high-brow")
fuzzel[FuzzyAutocompleteDistance] = function(str, ...)
	return FuzzySort(str, fuzzel.dld, tru, ...)
end
fuzzel.fad = fuzzel[FuzzyAutocompleteDistance]

--- Sorts truncated versions of inputed strings by ratio.
-- truncates each input string, and finds the Damerau-Levenshtein ratio of each string to the first argument, and sorts them into a table accordingly. Useful for auto-complete functions
-- @function FuzzyAutocompleteRatio
-- @string str the string to compare each result to
-- @param ... either a 1-indexed table, or a list of strings to sort
-- @return a 1-indexed table of the input strings, in the order of closest-to str to farthest-from str
-- @usage fuzzel.FuzzyAutocompleteRatio("egg",{"eggman","excelent","excaliber"})
-- @usage fuzzel.FuzzyAutocompleteRatio("egg","eggman","excelent","excaliber")
fuzzel[FuzzyAutocompleteRatio] = function(str, ...)
	return FuzzySort(str, fuzzel.dlr, tru, ...)
end
fuzzel.far = fuzzel[FuzzyAutocompleteRatio]

return fuzzel
