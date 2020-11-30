set -l FZF_NON_COLOR_OPTS

for arg in (echo $FZF_DEFAULT_OPTS | tr " " "\n")
    if not string match -q -- "--color*" $arg
        set -a FZF_NON_COLOR_OPTS $arg
    end
end

set -gx FZF_DEFAULT_OPTS "$FZF_NON_COLOR_OPTS"\
" --color=bg+:#{color11.strip},bg:#{background.strip},spinner:#{color14.strip},hl:#{color12.strip}"\
" --color=fg:#{foreground.strip},header:#{color12.strip},info:#{background.strip},pointer:#{color14.strip}"\
" --color=marker:#{color14.strip},fg+:#{background.strip},prompt:#{color3.strip},hl+:#{color12.strip}"

