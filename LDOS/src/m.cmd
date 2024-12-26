@set path=%path%;..\vasm\
vc -O2 -notmpfile -nostdlib -o ..\hdd_loader.exe hdd_loader.asm
vasmm68k_mot -Fbin -o p61player.bin p61player.asm
..\as68 boot.asm -o ..\boot.bin 
..\as68 boot2.asm -o ..\boot2.bin 
..\as68 kernel.asm -o ..\kernel.bin
copy kernel.inc ..
copy ..\hdd_loader.exe ..\..\DeProfundis\HDD_Version
pause
rem cd ..\..\LDOS_Test
rem call m_all.cmd
rem copy demo.adf X:\leonard\AMIGA\LeonardAmigaHD\travail
