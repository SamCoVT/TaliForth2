-- Mesen 2 TaliForth PCE Bridge
local TERM_OUT  = 0x3FFE
local TERM_STAT = 0x3FFF

print("--- TaliForth2 Bridge Active ---")

local function onWriteStat(address, value)
    -- value is what the PCE is TRYING to write (likely 0x01)
    if (value & 0x01) ~= 0 then
        -- 1. Read the character from $3FFE
        local charCode = emu.read(TERM_OUT, emu.memType.pceDebug)
        
        -- 2. Output to console
        local char = string.char(charCode)
        io.write(char)
        io.flush()
        
        -- 3. INTERCEPT: Return 0 to the emulator.
        -- This tells Mesen: "Don't write 01, write 00 instead."
        -- This clears the bit instantly before the CPU even finishes the STA instruction.
        return 0
    end
end

-- Registering specifically for the PCE main CPU and PCE Debug memory
-- emu.addMemoryCallback(callback, callbackType, start, end, cpuType, memoryType)
emu.addMemoryCallback(onWriteStat, emu.callbackType.write, TERM_STAT)

print("Intercepting writes to $3FFF...")