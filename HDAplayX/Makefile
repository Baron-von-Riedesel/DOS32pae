
ODIR=..\build
MODS=..\build\dosext.obj

all: $(ODIR) $(ODIR)\HDAplayX.exe

$(ODIR):
	@mkdir $(ODIR)

$(ODIR)\HDAplayX.exe: $(ODIR)\dos32pae.bin $(ODIR)\HDAplayX.obj $(ODIR)\dosext.obj Makefile
#	@link /nologo /subsystem:native $*.obj /stub:$(ODIR)\dos32pae.bin /fixed /stack:0x4000 /heap:0 /out:$*.exe /map:$*.map
	@jwlink format win pe ru native f $*,$(MODS) n $* op q,m=$*,stub=$(ODIR)\dos32pae.bin,stack=0x2000,heap=0x1000,norelocs

$(ODIR)\HDAplayX.obj: HDAplayX.asm printf.inc
	@jwasm -nologo -Fl$* -Fo$* -Sg -coff HDAplayX.asm

clean:
	@erase $(ODIR)\HDAplayX.exe
	@erase $(ODIR)\HDAplayX.obj
	@erase $(ODIR)\HDAplayX.map
	@erase $(ODIR)\HDAplayX.lst

