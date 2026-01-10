# TaliForth PCE Bridge v1.0

This project provides a development bridge between a Linux host and the PC Engine (TurboGrafx-16) running TaliForth. It allows you to use your modern terminal and, or a text editor (like Vim) to interact with the PCE hardware in real-time.

## Installation & Setup

### Step 1: Start the Bridge
The bridge script manages the communication pipes between your terminal and the emulator. Run this from your project root:

chmod +x platform/pce/bridge/forth-term.sh 
./platform/pce/bridge/forth-term.sh

Note: This script automatically creates /tmp/forth_in.txt and the /tmp/forth_out pipe.

### Step 2: Launch Mesen 2
1. Load your compiled taliforth.pce ROM.
2. Navigate to Tools > Lua Scripting.
3. Open and run platform/pce/bridge/mesen_bridge.lua.

## 3. Usage

### Interactive Mode
Type directly into the terminal running forth-term.sh. Your keystrokes are sent to the PCE, and the PCE's output is printed back to your terminal.

### File Injection (The Vim Workflow)
To send a block of code to the PCE without typing it manually:

cat my_code.fs > /tmp/forth_in.txt

Note: This file acts as a buffer you can paste code with anything and it will load instantly in the in your terminal. Dont paste in your forth shell directly the pcengine will not catch every byte.

Vim Tip: Add this to your .vimrc to send code with F5:
nnoremap <F5> :w !cat > /tmp/forth_in.txt<CR>

## 4. Technical Specifications

### I/O Memory Map
The bridge communicates via a specific memory window in the PCE address space:
- $3FFD: Data Input (Read by PCE)
- $3FFE: Data Output (Written by PCE)
- $3FFF: Status Register
  - Bit 0: Output Pending (PCE -> Host)
  - Bit 1: Input Ready (Host -> PCE)

### Communication
- Input: Host writes to /tmp/forth_in.txt. Lua reads and pushes to PCE.
- Output: PCE writes to $3FFE. Lua appends to /tmp/forth_out. Host tail -f reads the pipe.

## 5. Troubleshooting
- Stuck Terminal: If the bridge exits poorly and your terminal feels "broken," type 'stty sane' and press Enter.
- Ctrl+C: Use Ctrl+C to safely disconnect the bridge. The trap function will handle cleanup of background processes.
- Mesen Permissions: Ensure you turn on i/o permission setting for lua scripts.
