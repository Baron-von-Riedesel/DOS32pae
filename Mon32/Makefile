
ODIR=..\build

all: $(ODIR) $(ODIR)\mon32.exe

$(ODIR):
	@mkdir $(ODIR)

$(ODIR)\mon32.exe: $(ODIR)\dos32pae.bin $(ODIR)\mon32.obj
#	@link /nologo /subsystem:native $*.obj /stub:$(ODIR)\dos32pae.bin /fixed /stack:0x4000 /heap:0 /out:$*.exe /map:$*.map
	@jwlink format win pe ru native f $* n $* op q,m=$*,stub=$(ODIR)\dos32pae.bin,stack=0x4000,heap=0x1000,norelocs

$(ODIR)\mon32.obj: mon32.asm printf.inc
	@jwasm -nologo -Fl$* -Fo$* -Sg -coff -D?WAITINPM mon32.asm

clean:
	@erase $(ODIR)\mon32.exe
	@erase $(ODIR)\mon32.obj
	@erase $(ODIR)\mon32.map
	@erase $(ODIR)\mon32.lst

