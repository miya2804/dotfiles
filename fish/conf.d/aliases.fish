# LS
function ll
    ls -alhF $argv
end
function la
    ls -AF $argv
end
function l
    ls -CF $argv
end

# INTERACTIVE
function rm
    command rm -i $argv
end
function mv
    command mv -i $argv
end
function cp
    command cp -i $argv
end
