subroutine getcel(filnam    ,grpnam    ,nelems    ,elmnms    ,elmdms    , &
                & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
                & elmnam    ,celidt    ,wrilog    ,error     )
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
!    Function: Detect the number of time steps on filnam (map-
!              or his-file)
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use string_module
!
    implicit none
!
! Global variables
!
    integer         :: celidt !  Description and declaration in nefisio.igs
    integer         :: error
                                   !!  Error flag for NEFIS files
    integer         :: nelems
                                   !!  Number of elements in this cell and
                                   !!  group.
    integer, dimension(*) :: nbytsg
                                   !!  Array containing info about the size,
                                   !!  in bytes, of each element type
                                   !!  (ELMTPS). So for a REAL*4, this array
                                   !!  contains a 4. The size of the array
                                   !!  is (NELEMS).
    integer, dimension(6, *) :: elmdms !  Description and declaration in nefisio.igs
    logical, intent(in)            :: wrilog
                                   !!  Flag to write file
                                   !!    .TRUE. : write to  file
                                   !!    .FALSE.: read from file
    character(*)    :: elmnam
                                   !!  Name of element, who's values must
                                   !!  be written or read. This name must
                                   !!  be on of the set ELMNMS.
    character(*), intent(in)       :: filnam
                                   !!  Name for communication file
                                   !!  com-<case><label>
    character(*)    :: grpnam
                                   !!  Name of data group to write to or
                                   !!  to read from. This name is also
                                   !!  used as the name for the cell and
                                   !!  group-definition.
    character(*), dimension(*) :: elmdes
                                   !!  Array with element description (i.e.
                                   !!  cartesian coordinate etc., etc.)
    character(*), dimension(*) :: elmnms
                                   !!  Array with names of the elements to
                                   !!  be used in the cell
    character(*), dimension(*) :: elmqty
                                   !!  Array with element quantity (i.e.
                                   !!  length, force  etc., etc.)
    character(*), dimension(*) :: elmtps
                                   !!  Array with element types (i.e. REAL,
                                   !!  INTEGER etc., etc.)
    character(*), dimension(*) :: elmunt
                                   !!  Array with element physical unit
                                   !!  (i.e. m  m/s etc., etc.)
!
!
! Local variables
!
    integer                        :: datlen
    integer                        :: deflen
    integer                        :: fd_nef
    integer                        :: ierror
    integer                        :: ind
    integer, dimension(5)          :: grpdms
    integer                        :: grpndm
    integer, dimension(5)          :: grpord
    integer, external              :: clsnef
    integer, external              :: crenef
    integer, external              :: inqgrp
    integer, external              :: inqmxi
    integer, external              :: neferr
    character(1)                   :: coding
    character(134)                 :: errstr
    character(2)                   :: access
    character(256)                 :: datnam
    character(256)                 :: defnam
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initialization
    !
    grpndm = 5
    coding = 'N'
    fd_nef = -1
    !
    ! aggregate file names
    !
    ind = len_trim(filnam)+1
    !
    datnam = filnam
    datnam(ind:ind + 3) = '.dat'
    call remove_leading_spaces(datnam    ,datlen    )
    !
    defnam = filnam
    defnam(ind:ind + 3) = '.def'
    call remove_leading_spaces(defnam    ,deflen    )
    !
    ! write or read data from nefis files
    !
    if (wrilog) then
       access = 'u'
    else
       access = 'r'
    endif
    !
    error = crenef(fd_nef, datnam(1:datlen), defnam(1:deflen), coding, access)
    if (error/=0 .and. .not.wrilog) then
       error = -211
       goto 10000
    endif
    if (error/=0) goto 9999
    !
    ! Read name of the group definition of a data group from
    ! data file.
    !
    error = inqgrp(fd_nef, grpnam, grpnam, grpndm, grpdms, grpord)
    if (error==0) then
       celidt = grpdms(1)
       !
       ! Test value of celidt if celidt = 0 then get celidt with INQMXI
       !
       if (celidt<=0) then
          error = inqmxi(fd_nef, grpnam, celidt)
       endif
    endif
    if (error/=0 .or. celidt<0) celidt = 0
    !
    ! No error message needed when group does not exist (error == 6007)
    !
    if (error==6007) goto 10000
 9999 continue
    if (error/=0) ierror = neferr(1, errstr)
10000 continue
    ierror = clsnef(fd_nef)
end subroutine getcel
