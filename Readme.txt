
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
   - int 31h, ax=203h: set exception vector BL to CX:EDX
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
  conventional memory). The stack's located just behind the image. As default,
  the IDT is mapped at linear address  0x100000, just above conventional memory.
  Paging tables aren't mapped at all - hence the full address space from 
  0x101000 up to 0xffffffff may be used by the application.


  5. License
  
   The source is distributed under the MIT license. See file license.txt for
  details. It's written by Andreas Grech.
