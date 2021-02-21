
ODIR=..\build

all: $(ODIR) $(ODIR)\dos32pae.bin

$(ODIR):
	@mkdir $(ODIR)

$(ODIR)\dos32pae.bin: dos32pae.asm Makefile
	@jwasm -nologo -mz -W3 -Fl$* -Fo$*.bin dos32pae.asm
#	@jwasm -nologo -Fl$* -Fo$* dos32pae.asm
#	@jwlink format dos f $* n $*.bin op q,m=$*,knoweas

clean:
	erase $(ODIR)\dos32pae.bin
	erase $(ODIR)\dos32pae.lst
