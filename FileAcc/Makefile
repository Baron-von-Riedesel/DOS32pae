
ODIR=..\build
MODS=..\build\dosext.obj

all: $(ODIR) $(ODIR)\FileAcc.exe

$(ODIR):
	@mkdir $(ODIR)

$(ODIR)\FileAcc.exe: $(ODIR)\dos32pae.bin $(ODIR)\FileAcc.obj $(ODIR)\dosext.obj Makefile
#	@link /nologo /subsystem:native $*.obj /stub:$(ODIR)\dos32pae.bin /fixed /stack:0x4000 /heap:0 /out:$*.exe /map:$*.map
	@jwlink format win pe ru native f $*,$(MODS) n $* op q,m=$*,stub=$(ODIR)\dos32pae.bin,stack=0x2000,heap=0x1000,norelocs

$(ODIR)\FileAcc.obj: FileAcc.asm printf.inc
	@jwasm -nologo -Fl$* -Fo$* -Sg -coff FileAcc.asm

clean:
	@erase $(ODIR)\FileAcc.exe
	@erase $(ODIR)\FileAcc.obj
	@erase $(ODIR)\FileAcc.map
	@erase $(ODIR)\FileAcc.lst

