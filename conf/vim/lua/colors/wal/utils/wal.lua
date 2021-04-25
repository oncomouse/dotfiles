local json = require('colors.wal.utils.json')
local read_file = require('colors.wal.utils.read_file')
local function get_wal_theme()
	local xdg_cache_home = os.getenv('XDG_CACHE_HOME') or os.getenv('HOME') .. '/.cache'
	local pywal_cache = xdg_cache_home .. '/wal/colors.json'
	local json_scheme = json.parse(read_file(pywal_cache))
	return json_scheme
end

return get_wal_theme
