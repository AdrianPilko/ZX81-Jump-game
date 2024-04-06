REM build all the assembly code "main" files in this directory
REM clean up before calling assembler 
del jump.p
del *.lst
del *.sym

call zxasm jump

REM call will auto run emulator EightyOne if installed
REM comment in or out usin rem which one to run

if exist jump.p (
  call jump.p
  exit
) else (
  pause
)



