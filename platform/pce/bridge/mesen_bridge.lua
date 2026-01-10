-- Mesen 2 TaliForth PCE Bridge
local TERM_IN   = 0x3FFD
local TERM_OUT  = 0x3FFE
local TERM_STAT = 0x3FFF
local INPUT_FILE = "/tmp/forth_in.txt"

local inputBuffer = ""

-- 1. OUTPUT: PCE -> Linux /tmp/forth_out
local function onWriteStat(address, value)
    if (value & 0x01) ~= 0 then
        local charCode = emu.read(TERM_OUT, emu.memType.pceDebug)
        local f = io.open("/tmp/forth_out", "a")
        if f then
            f:write(string.char(charCode))
            f:close()
        end
        return value & 0xFE -- Clear Bit 0
    end
    return value
end

-- 2. STATUS: PCE checking if data is ready at $3FFF
local function onReadStat(address, value)
    if #inputBuffer > 0 then
        -- Set Bit 1 (Input Ready) without touching other bits
        return value | 0x02  
    else
        -- Clear Bit 1 (Input Empty) without touching other bits
        -- 0xFD is the bitmask for everything EXCEPT Bit 1
        return value & 0xFD
    end
end

-- 3. DATA INPUT: PCE reading the actual byte at $3FFD
local function onReadData(address, value)
    if #inputBuffer > 0 then
        -- Get the current char
        local char = inputBuffer:byte(1)
        
        -- Remove it from the buffer
        inputBuffer = inputBuffer:sub(2)
        return char
    end
    return 0
end

-- 4. UPDATED POLLER: Pre-loads the first char into memory
local function pollInputFile()
    local f = io.open(INPUT_FILE, "r")
    if f then
        local content = f:read("*a")
        f:close()
        if content and #content > 0 then
            local clear = io.open(INPUT_FILE, "w")
            if clear then clear:close() end
            
            -- Add to buffer (convert to Forth CR)
            inputBuffer = inputBuffer .. content:gsub("\n", "\r")
        end
    end
end

-- Callbacks
emu.addMemoryCallback(onWriteStat, emu.callbackType.write, TERM_STAT)
emu.addMemoryCallback(onReadStat, emu.callbackType.read, TERM_STAT)
emu.addMemoryCallback(onReadData, emu.callbackType.read, TERM_IN)
emu.addEventCallback(pollInputFile, emu.eventType.endFrame)