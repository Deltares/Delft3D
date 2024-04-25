!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!-------------------------------------------------------------------------------
!   Delft-FSM (Fortran Shared Memory)
!   Interface between Fortran (the users of FSM) and C (the implementation)
!
!   The FSM API functions provided in this Fortran file are:
!       FSMINI  -> FSM_Init
!       MAKPTR  -> FSM_MakePointer
!       GETPTR  -> FSM_GetPointer
!       RELPTR  -> FSM_ReleasePointer
!       PRTKEY  -> FSM_PrintKeys
!       FSMERR  -> FSM_Err
!
!   This file also contains a few Fortran helper functions:
!       fsm_check_alignment
!       fsm_pointer_index
!       fsm_set_error
!
!   Irv.Elshoff@deltares.nl
!   3 aug 06
!
!-------------------------------------------------------------------------------


integer function FSMINI (contextid, flags)
    use precision
    implicit none
    integer contextid
    integer flags

    include 'globals-fsm.i'

    FSMINI = FSM_Init (contextid, flags)
    return
end


!-------------------------------------------------------------------------------


function MAKPTR (keyname, keytype, length)
    use precision
    implicit none
    character   keyname*(*)
    integer keytype
    integer length

    !   This function allocates memory for different types of data.
    !   These types can be either Integer, Real, Double Precision,
    !   Complex, Double Complex, Logical or Character.
    !   The allocated pointer is stored with the key name in
    !   an internal table for later use.
    !   If the memory allocation is okay, this function returns
    !   an index in either array IBUF, RBUF or DBUF, CBUF, DCBUF
    !   LBUF or CHBUF, depending on the specified type.
    !   These arrays are in common /DYNMEM/.
    !   Otherwise, zero is returned.

    include 'globals-fsm.i'

    integer           lkey
    character         ckey*2000
    integer(pntrsize) MAKPTR
    integer(pntrsize) ptr
    integer(pntrsize) fsm_pointer_index

    !   Convert key name string to null-terminated C representation.

    lkey = index (keyname ,' ') - 1
    if (lkey .eq. -1) lkey = len (keyname)
    ckey = keyname (1:lkey) // char(0)

    !   Allocate the requested memory

    ptr = FSM_MakePointer (ckey, length, keytype, nbytes(keytype))

    !   Convert raw memory address to Fortran array index
    !   if the allocate succeeds

    if (ptr .eq. 0) then
        MAKPTR = 0
    else
        MAKPTR = fsm_pointer_index (ptr, keytype)
    end if

    return
end


!-------------------------------------------------------------------------------

function GETPTR (keyname)
    use precision
    implicit none
    character   keyname*(*)

    !   This function returns an index in either array IBUF, RBUF,
    !   DBUF, CBUF, DCBUF, LBUF or CHBUF, depending on type,
    !   for the given key.  If the key is not found, zero is returned

    include 'globals-fsm.i'

    integer(pntrsize) GETPTR
    integer           lkey
    character         ckey*2000
    integer           keytype
    integer(pntrsize) ptr
    integer(pntrsize) fsm_pointer_index

    !   Convert key name string to null-terminated C representation.

    lkey = index (keyname ,' ') - 1
    if (lkey .eq. -1) lkey = len (keyname)
    ckey = keyname (1:lkey) // char(0)

    !   Lookup the previously allocated memory block

    keytype = 0
    ptr = FSM_GetPointer (ckey, keytype)

    if (ptr .eq. 0) then
        GETPTR = 0
    else
        GETPTR = fsm_pointer_index (ptr, keytype)
    endif

    return
end


!-------------------------------------------------------------------------------

integer function RELPTR (keyname)
    use precision
    implicit none
    character   keyname*(*)

    include 'globals-fsm.i'

    integer           lkey
    character         ckey*2000
    integer           keytype
    integer(pntrsize) ptr
    integer(pntrsize) fsm_pointer_index

    !   Convert key name string to null-terminated C representation.

    lkey = index (keyname ,' ') - 1
    if (lkey .eq. -1) lkey = len (keyname)
    ckey = keyname (1:lkey) // char(0)

    !   Lookup the previously allocated memory block

    keytype = 0
    ptr = FSM_ReleasePointer (ckey, keytype)

    if (ptr .eq. 0) then
        RELPTR = 0
    else
        RELPTR = fsm_pointer_index (ptr, keytype)
    endif
    return
end


!-------------------------------------------------------------------------------


integer function PRTKEY ()
    use precision
    implicit none
    include 'globals-fsm.i'
    integer status
    
    status = FSM_PrintKeys ()
    PRTKEY = 0
    return
end


!-------------------------------------------------------------------------------


integer function FSMERR (string)
    use precision
    implicit none
    character string*(*)

    include 'globals-fsm.i'
    integer status
 
    status = FSM_Err (string, len(string))
    FSMERR = 0
    return
end


!-------------------------------------------------------------------------------


integer function FSMTRF (filename)
    use precision
    implicit none
    character filename*(*)

    include 'globals-fsm.i'

    integer     lfile
    character   cfile*2000

    !   Convert file name string to null-terminated C representation.

    lfile = index (filename ,' ') - 1
    if (lfile .eq. -1) lfile = len (filename)
    cfile = filename (1:lfile) // char(0)

    !   Invoke C routine to do the actual work

    FSMTRF = FSM_TraceFile (cfile)
    return
end


!-------------------------------------------------------------------------------
!   Each of the seven data types that can be allocated have an element size
!   and an alignment requirement, both in bytes.  These numbers are stored
!   in two global arrays and initialized below.


block data FSMBYT
    use precision
    implicit none
    common /fsmglo/ nbytes, alignment

    integer nbytes (7)
    integer(pntrsize) alignment (7)

    data nbytes     /  4,  4,  8, 16,  8,  4,  1 /
    data alignment  / 16,  4,  8, 16, 16,  8,  4 /

    !   Note: these size and alignment values are duplicated in "globals.h"
    !   and must match the values there

end


!-------------------------------------------------------------------------------
!   The following helper routines are not intended to be called
!   by FSM user programs.


subroutine fsm_check_alignment
    use precision
    implicit none

    !   This routine is called once at the start of a program that uses FSM
    !   to ensure that the base addresses of the arrays used to reference
    !   allocated memory comply to the alignment requirements.

    include 'fsm.i'
    include 'globals-fsm.i'

    integer(pntrsize) location

    location = loc (ibuf (0))
    if (mod (location, alignment (ityp)) .ne. 0) then
        call alignment_error ('Integer')
    endif

    location = loc (rbuf (0))
    if (mod (location, alignment (rtyp)) .ne. 0) then
        call alignment_error ('Real')
    endif

    location = loc (dbuf (0))
    if (mod (location, alignment (dtyp)) .ne. 0) then
        call alignment_error ('Double precision')
    endif

    location = loc (dcbuf (0))
    if (mod (location, alignment (dctyp)) .ne. 0) then
        call alignment_error ('Double complex')
    endif

    location = loc (cbuf (0))
    if (mod (location, alignment (ctyp)) .ne. 0) then
        call alignment_error ('Complex')
    endif

    location = loc (lbuf (0))
    if (mod (location, alignment (ltyp)) .ne. 0) then
        call alignment_error ('Logical')
    endif

    location = loc (chbuf (0))
    if (mod (location, alignment (chtyp)) .ne. 0) then
        call alignment_error ('Character')
    endif
end


!-------------------------------------------------------------------------------


function fsm_pointer_index (ptr, keytype)
    use precision
    implicit none

    !   This function contains the main "gimmick" of FSM: return an
    !   index to an array in the DYNMEM common block which, when used
    !   to index the array, results in a reference to dynamically allocated
    !   memory.

    include 'fsm.i'
    include 'globals-fsm.i'

    integer(pntrsize) ptr
    integer           keytype
    integer(pntrsize) fsm_pointer_index
    integer(pntrsize) base

    !   Relate return address to an index in the appropriate array.

    if (keytype .eq. ityp) then
        base = loc (ibuf (0))
    else if (keytype .eq. rtyp) then
        base = loc (rbuf (0))
    else if (keytype .eq. dtyp) then
        base = loc (dbuf (0))
    else if (keytype .eq. ctyp) then
        base = loc (cbuf (0))
    else if (keytype .eq. dctyp) then
        base = loc (dcbuf(0))
    else if (keytype .eq. ltyp) then
        base = loc (lbuf (0))
    else if (keytype .eq. chtyp) then
        base = loc (chbuf(0))
    endif

    fsm_pointer_index = ((ptr - base) / nbytes(keytype))
end

!--------------------------------------------------------------------------------
integer function ESMINIF (flags)
    use precision
    implicit none

    integer flags

    include 'globals-fsm.i'
    integer, external :: esm_init_f
    
    ESMINIF = ESM_Init_F (flags)
    return
end
!--------------------------------------------------------------------------------
integer function ESMCREATEF (shared,psize)
    use precision
    implicit none

    integer shared
    integer psize

    include 'globals-fsm.i'
    integer, external :: esm_create_f
    
    ESMCREATEF = ESM_Create_F (shared, psize)
    return
end


!-------------------------------------------------------------------------------


subroutine alignment_error (type)
    use precision
    implicit none
    character   type*(*)

    include 'globals-fsm.i'

    character   error*2000
    integer     status

    error = 'FSM Error: ' // &
                type (1:len (type)) // &
                ' buffer is not aligned in memory properly' // &
                char(0)

    write (*,*) 'FATAL ERROR, Program Aborting...'
    write (*,*) trim(error)
    call throwexception()
end
