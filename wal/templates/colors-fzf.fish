set -l FZF_NON_COLOR_OPTS

for arg in (echo $FZF_DEFAULT_OPTS | tr " " "\n")
    if not string match -q -- "--color*" $arg
        set -a FZF_NON_COLOR_OPTS $arg
    end
end
set -Ux FZF_COLORS " --color=bg+:{color8},bg:{background},spinner:{color14},hl:{color12}"\
" --color=fg:{foreground},header:{color12},info:{background},pointer:{color14}"\
" --color=marker:{color14},fg+:{foreground},prompt:{color3},hl+:{color12}"

set -Ux FZF_DEFAULT_OPTS "$FZF_NON_COLOR_OPTS $FZF_COLORS"

