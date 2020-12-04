set -l FZF_NON_COLOR_OPTS

for arg in (echo $FZF_DEFAULT_OPTS | tr " " "\n")
    if not string match -q -- "--color*" $arg
        set -a FZF_NON_COLOR_OPTS $arg
    end
end
set -Ux FZF_COLORS " --color=bg+:#{color8.strip},bg:#{background.strip},spinner:#{color14.strip},hl:#{color12.strip}"\
" --color=fg:#{foreground.strip},header:#{color12.strip},info:#{background.strip},pointer:#{color14.strip}"\
" --color=marker:#{color14.strip},fg+:#{foreground.strip},prompt:#{color3.strip},hl+:#{color12.strip}"

set -Ux FZF_DEFAULT_OPTS "$FZF_NON_COLOR_OPTS $FZF_COLORS"

