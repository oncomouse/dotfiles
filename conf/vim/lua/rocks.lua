local home = os.getenv("HOME")

if home == nil then
    return
end

local path = table.concat({
    '/usr/share/lua/5.1/?.lua',
    '/usr/share/lua/5.1/?/init.lua',
    '/usr/lib/lua/5.1/?.lua',
    '/usr/lib/lua/5.1/?/init.lua',
    './?.lua',
    './?/init.lua',
    '~/.luarocks/share/lua/5.1/?.lua',
    '~/.luarocks/share/lua/5.1/?/init.lua'
}, ";")

local cpath = table.concat({
    '/usr/lib/lua/5.1/?.so',
    '/usr/lib/lua/5.1/loadall.so',
    './?.so',
    '~/.luarocks/lib/lua/5.1/?.so'
}, ";")

package.path = path:gsub("~", home)
package.cpath = cpath:gsub("~", home)
