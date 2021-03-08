
  1. About Dos32pae

   Dos32pae is a program that is supposed to be added (by a linker) to
  a 32-bit PE binary - a so-called "stub". The stub is executed when
  the binary is launched in DOS. Dos32pae does the following:

   - check if PAE paging is supported
   - check if the PE image is "acceptable" (see below)
   - check if enough XMS memory is available for image & paging
   - setup IDT and page tables for 32-bit PAE paging
   - read & move the image into extended memory
   - reprogram master PIC, so IRQs 00h-07h are mapped to Int 78h-7fh
   - install a small protected-mode "OS" (int 21h/31h)
   - switch to protected-mode
   - call the entry point of the loaded 32-bit image


  2. Requirements
  
  to run an image with Dos32pae attached one needs:

   - a CPU supporting PAE paging
   - an installed DOS
   - an installed XMS host
   - enough extended memory to load the image


  3. How to use Dos32pae

   The stub is added to a 32-bit binary thru the link step. See the 
  Makefiles supplied for the samples how to do this with MS link or jwlink.
  The image must meet the following requirements:

   - Subsystem has to be "native"; avoids the image being loaded as "Windows"
     application.
   - no dll references ("imports") are possible

   There are a few samples supplied:

   - Alloc.asm; this sample shows how to use the memory "management" of
     the stub. It allocates a huge linear address space and backfills it
     with extended memory allocated thru XMS v3.5 (HimemSX).

   - Mon32.asm; this sample allows to display linear memory.

   - FileAcc.asm; this sample uses extension module dosext.obj, that installs
     int 21h extensions for file IO, so one can use int 21h directly for 
     file access.

   - HDAplayX.asm; this is actually a "real" application. It plays audio
     files, accessing the HDA (High Definition Audio) controller directly.
     Additionally, for storing the samples it uses XMS v3.5 to allocate
     extended memory beyond the 4 GB barrier. So it will only run if HimemSX
     is installed and the HDA controller can handle 64-bit addresses.


  4. Technical Details

  a) Overview

   The 32-bit binary runs in ring 0, 32-bit protected mode with PAE paging
  enabled. PAE paging is not compatible with legacy 32-bit paging, hence
  Dos32pae won't run as VCPI client within V86-monitors like (J)EMM386!
  Also, since it's no DPMI ( although somewhat DPMI-like ), the binary won't
  run in DOS-Boxes - it needs plain DOS!

  b) DPMI API

   Dos32pae installs a tiny subset of the DPMI API. The functions that are
  supported are:
   - int 21h, ah=4Ch: terminate program
   - int 31h, ax=202h: get exception vector BL in CX:EDX
   - int 31h, ax=203h: set exception vector BL to CX:EDX
   - int 31h, ax=204h: get interrupt vector BL in CX:EDX
   - int 31h, ax=205h: set interrupt vector BL to CX:EDX
   - int 31h, ax=300h: simulate real-mode interrupt BL, EDI -> RMCS.
   - int 31h, ax=301h: call real-mode far proc with RETF frame, EDI -> RMCS.
   - int 31h, ax=504h: allocate uncommitted memory (=address space).

   Additionally, Dos32pae supports a new DPMI function:
   - int 31h, ax=518h: map physical region into address space. This function
     is a variant of function 508h, the difference is that the physical address
     may be 64-bit (in EDI:EDX).

  c) Memory Layout

   Dos32pae offers a very limited memory management: it handles linear address
  space only. If the application wants to dynamically allocate committed
  memory, it has to allocate address space and then backfill the space with
  physical memory ( see sample Alloc.asm how this is done ). It's a bit more 
  complicated than the usual DPMI way, but has the advantage that any physical
  memory can be "mapped in" - even memory beyond the 4 GB barrier.

   The PE image that Dos32pae is bound to will be loaded at its preferred load
  address, so no relocations are needed ( the load address must not be in 
  conventional memory). The stack's located just behind the image.
  
  As default, the first MB of memory ("conventional" memory) is mapped at
  0x00000-0xFFFFF. Behind that comes the IDT, at linear address 0x100000.
  Begin and size of "conventional" memory can be changed to some degree, 
  though. It's possible to reduce the size if no BIOS access is needed in
  protected-mode, or access to page 0 can be disabled to detect null-pointers.

  Paging tables aren't mapped at all - hence the full address space above
  the IDT up to 0xffffffff may be used by the application.


  5. How to build Dos32pae

  The Dos32pae stub and all supporting modules (dosext.obj) are written in 
  16-/32-bit Masm-style assembly language. The recommended tools to create the
  binaries are JWasm and JWlink. The Makefiles supplied are in MS NMake style -
  but if the tool isn't available, Open Watcom's wmake may do the job as well.


  6. License
  
   The source is distributed under the MIT license. See file license.txt for
  details. It's written by Andreas Grech.

