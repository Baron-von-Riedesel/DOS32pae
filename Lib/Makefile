
ODIR=..\build
DEBUG=0

!if $(DEBUG)
AOPTD= -D_DEBUG
!else
AOPTD=
!endif

all: $(ODIR) $(ODIR)\dosext.obj

$(ODIR):
	@mkdir $(ODIR)

$(ODIR)\dosext.obj: dosext.asm Makefile
	@jwasm -nologo -coff -Sg -Fl$* -Fo$* $(AOPTD) dosext.asm

clean:
	erase $(ODIR)\dosext.obj
	erase $(ODIR)\dosext.lst
