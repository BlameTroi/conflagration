# __________________gdb options_________________

set print pretty on
# tui enable

set confirm off
set verbose off
set history filename ~/.gdb_history
set history save

set disassembly-flavor intel

# hex input output
#set output-radix 0x10
#set input-radix 0x10

# These make gdb never pause in its output
set height 0
set width 0

# one way to get a dump the way i like
define xxd
    dump binary memory dump.bin $arg0 $arg0+$arg1
    shell xxd dump.bin
end
