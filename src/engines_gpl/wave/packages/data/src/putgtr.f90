subroutine putgtr(filnam    ,grpnam    ,nelems    ,elmnms    ,elmdms    , &
                & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                & elmnam    ,celidt    ,wrilog    ,error     ,buffr     )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!!--description-----------------------------------------------------------------
!
!    Function: Read or writes character buffer to nefis files
!              Tests values input consistency for elmnam and
!              elmnms and for local and global dimensions
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    integer, parameter :: start = 1
    integer, parameter :: stopp = 2
    integer, parameter :: incr  = 3
!
! Global variables
!
    integer                , intent(in)  :: celidt !  Description and declaration in nefisio.igs
    integer                              :: error  !!  Error flag for NEFIS files
    integer                              :: nelems !!  Number of elements in this cell and
                                                   !!  group.
    integer, dimension(*)                :: nbytsg !!  Array containing info about the size,
                                                   !!  in bytes, of each element type
                                                   !!  (ELMTPS). So for a REAL*4, this array
                                                   !!  contains a 4. The size of the array
                                                   !!  is (NELEMS).
    integer, dimension(6, *)             :: elmdms !  Description and declaration in nefisio.igs
    logical                , intent(in)  :: wrilog !!  Flag to write file
                                                   !!    .TRUE. : write to  file
                                                   !!    .FALSE.: read from file
    real, dimension(*)                   :: buffr  !!  User supplied buffer to read from or
                                                   !!  to write to, depending on the write
                                                   !!  switch (WRILOG).
    character(*)                         :: elmnam !!  Name of element, who's values must
                                                   !!  be written or read. This name must
                                                   !!  be on of the set ELMNMS.
    character(*)           , intent(in)  :: filnam !!  Name for communication file
                                                   !!  com-<case><label>
    character(*)                         :: grpnam !!  Name of data group to write to or
                                                   !!  to read from. This name is also
                                                   !!  used as the name for the cell and
                                                   !!  group-definition.
    character(*), dimension(*)           :: elmdes !!  Array with element description (i.e.
                                                   !!  cartesian coordinate etc., etc.)
    character(*), dimension(*)           :: elmnms !!  Array with names of the elements to
                                                   !!  be used in the cell
    character(*), dimension(*)           :: elmqty !!  Array with element quantity (i.e.
                                                   !!  length, force  etc., etc.)
    character(*), dimension(*)           :: elmtps !!  Array with element types (i.e. REAL,
                                                   !!  INTEGER etc., etc.)
    character(*), dimension(*)           :: elmunt !!  Array with element physical unit
                                                   !!  (i.e. m  m/s etc., etc.)
!
! Local variables
!
    integer                        :: buflen
    integer                        :: elmndm
    integer                        :: fd_nef
    integer                        :: i
    integer                        :: ierror
    integer                        :: j
    integer                        :: lelmnr
    integer                        :: n
    integer, dimension(5)          :: elmdim
    integer, dimension(5)          :: uindex
    integer, external              :: clsnef
    integer, external              :: credat
    integer, external              :: crenef
    integer, external              :: defcel
    integer, external              :: defelm
    integer, external              :: defgrp
    integer, external              :: getelt
    integer, external              :: inqelm
    integer, external              :: neferr
    integer, external              :: putelt
    character(1)                   :: coding
    character(16)                  :: elmant
    character(16)                  :: elmqta
    character(2)                   :: access
    character(256)                 :: datnam
    character(256)                 :: defnam
    character(256)                 :: errmsg
    character(64)                  :: elmdas
    character(8)                   :: elmtap
!
!! executable statements -------------------------------------------------------
!
    !-----Initialization
    !
    coding        = 'N'
    uindex(start) = celidt
    uindex(stopp) = celidt
    uindex(incr)  = 1
    fd_nef        = -1
    elmndm        = 5
    !
    !-----aggregate file names
    !
    datnam = trim(adjustl(filnam))//'.dat'
    defnam = trim(adjustl(filnam))//'.def'
    !
    !-----write or read data from nefis files
    !
    if (wrilog) then
       access = 'u'
    else
       access = 'r'
    endif
    !
    error = crenef(fd_nef, trim(adjustl(datnam)), trim(adjustl(defnam)), coding, access)
    if (error/=0 .and. .not.wrilog) then
       error = -211
       goto 10000
    endif
    if (error/=0) goto 9999
    !
    if (wrilog) then
       error = putelt(fd_nef, grpnam, elmnam, uindex, 1, buffr)
    else
       j = 0
  123  continue
       j = j + 1
       if (elmnam==elmnms(j)) then
          !-------size single precision integer
          buflen = nbytsg(j)
          do i = 1, elmdms(1, j)
             buflen = buflen*elmdms(i + 1, j)
          enddo
          error = getelt(fd_nef, grpnam, elmnam, uindex, 1, buflen, buffr)
          if (error/=0) goto 9999
          goto 100
       endif
       goto 123
    endif
    !
    !-----error:
    !     writing: most likely error non existing group, so define it
    !     reading: error, no error expected
    !
  100 continue
    if (error/=0 .and. wrilog) then
       !-------Create elements
       do lelmnr = 1, nelems
          error = defelm(fd_nef, elmnms(lelmnr), elmtps(lelmnr), nbytsg(lelmnr),&
                & elmqty(lelmnr), elmunt(lelmnr), elmdes(lelmnr),               &
                & elmdms(1, lelmnr), elmdms(2, lelmnr))
          !---------most likely error, element already exist
          error = 0
       enddo
       !-------Create cells
       error = defcel(fd_nef, grpnam, nelems, elmnms)
       if (error/=0) goto 9999
       !-------Create group on definition file
       error = defgrp(fd_nef, grpnam, grpnam, 1, 0, 1)
       if (error/=0) goto 9999
       !-------Create group on data file
       error = credat(fd_nef, grpnam, grpnam)
       if (error/=0) goto 9999
       !-------try again to write data
       error = putelt(fd_nef, grpnam, elmnam, uindex, 1, buffr)
       if (error/=0) goto 9999
    endif
    !
    !-----No error when reading elements
    !
    if (error==0 .and. .not.wrilog) then
       error = inqelm(fd_nef, elmnam, elmtap, buflen, elmqta, elmant, elmdas,   &
             & elmndm, elmdim)
       !
       if (error/=0) goto 9999
       lelmnr = 0
       do n = 1, nelems
          if (elmnam==elmnms(n)) then
             lelmnr = n
             exit
          endif
       enddo
       if (lelmnr/=0) goto 9999
       !
       do i = 1, elmndm
          !
          !---------Compare local and global dimensions, not equal
          !         => new error number and exit
          !
          if (elmdim(i)/=elmdms(1 + i, lelmnr)) then
             error = -15025
             goto 9999
          endif
       enddo
    endif
    goto 10000
    !
 9999 continue
    if (error/=0) ierror = neferr(1, errmsg)
10000 continue
    ierror = clsnef(fd_nef)
end subroutine putgtr
