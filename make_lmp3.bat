:: gcc -shared -Wl,--add-stdcall-alias -O3 -march=core2 -flto -fomit-frame-pointer -fexpensive-optimizations -funroll-loops "lmp3.c" -o "lmp3.dll"
gcc -shared -Wl,--add-stdcall-alias -O3 -flto -fomit-frame-pointer -fexpensive-optimizations -funroll-loops "lmp3.c" -o "lmp3.dll"
:: -march=corei7
:: -march=corei7
:: -march=native 
pause