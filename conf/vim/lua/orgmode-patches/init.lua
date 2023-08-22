local patches = vim.fs.find(function(fn)
    if fn:match(".*%init.lua$") then
        return false
    end
    return fn:match(".*%.lua$")
end, {
    limit = math.huge,
    type = "file",
    path = vim.fs.dirname(debug.getinfo(1, "S").source:sub(2)),
})

for _, patch in ipairs(patches) do
    require("orgmode-patches." .. vim.fs.basename(patch:gsub("%.lua$", "")))
end
