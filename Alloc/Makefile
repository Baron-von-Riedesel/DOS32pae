
ODIR=..\build

all: $(ODIR) $(ODIR)\Alloc.exe

$(ODIR):
	@mkdir $(ODIR)

$(ODIR)\Alloc.exe: $(ODIR)\dos32pae.bin $(ODIR)\Alloc.obj Makefile
#	@link /nologo /subsystem:native $*.obj /stub:$(ODIR)\dos32pae.bin /fixed /stack:0x4000 /heap:0 /out:$*.exe /map:$*.map
	@jwlink format win pe ru native f $* n $* op q,m=$*,stub=$(ODIR)\dos32pae.bin,offset=0x200000,stack=0x4000,heap=0x1000,norelocs

$(ODIR)\Alloc.obj: Alloc.asm printf.inc
	@jwasm -nologo -Fl$* -Fo$* -Sg -coff Alloc.asm

clean:
	@erase $(ODIR)\Alloc.exe
	@erase $(ODIR)\Alloc.obj
	@erase $(ODIR)\Alloc.map
	@erase $(ODIR)\Alloc.lst

