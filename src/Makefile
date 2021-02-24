
ODIR=..\build
DEBUG=0

!if $(DEBUG)
AOPTD= -D_DEBUG
!else
AOPTD=
!endif

all: $(ODIR) $(ODIR)\dos32pae.bin

$(ODIR):
	@mkdir $(ODIR)

$(ODIR)\dos32pae.bin: dos32pae.asm Makefile
	@jwasm -nologo -mz -W3 -Sg -Fl$* -Fo$*.bin $(AOPTD) dos32pae.asm
#	@jwlink format dos f $* n $*.bin op q,m=$*,knoweas

clean:
	erase $(ODIR)\dos32pae.bin
	erase $(ODIR)\dos32pae.lst
