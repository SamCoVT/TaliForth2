\ platform_forth.fs - PCE Startup

\ Splash Screen
\ This is displayed during the COLD boot process.
cr .( Are you one of us, or one of them? PCEngine! ) cr

\ ------------------------------------------------------------------------
\ Custom PCE Forth words can be added below.
\ Note: There is a 16 bit char limit for this file in the default ROM build.
\ (After packing scrip) all code will still be compiled in ram. 
\ ------------------------------------------------------------------------

