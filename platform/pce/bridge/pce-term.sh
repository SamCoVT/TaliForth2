#!/bin/bash
IN="/tmp/forth_in.txt"
OUT="/tmp/forth_out"

# Gen files if missing
if [ ! -f "$IN" ]; then
    touch "$IN"
    echo "Created input file: $IN"
fi

if [ ! -p "$OUT" ]; then
    rm -f "$OUT" # Remove if it was a regular file
    mkfifo "$OUT"
    echo "Created output pipe: $OUT"
fi

# Start the output in the background to the pipe
tail -f "$OUT" &
WATCHER_PID=$!

# Cleanup on exit (now WATCHER_PID is actually set)
trap "stty sane; kill $WATCHER_PID; echo -e '\nDisconnected.'; exit" SIGINT SIGTERM

# SET TERMINAL MODE
# raw: individual keystrokes
# -echo: no double-type
# opost onlcr: fix the staircase
stty raw -echo opost onlcr isig

echo "--- TaliForth PCE Bridge (Pipe Mode) ---"

# INPUT LOOP
while true; do
    # Read 1 raw character from your keyboard
    char=$(dd bs=1 count=1 2>/dev/null)
    
    # Send it to the Forth input file
    echo -n "$char" > "$IN"
done
