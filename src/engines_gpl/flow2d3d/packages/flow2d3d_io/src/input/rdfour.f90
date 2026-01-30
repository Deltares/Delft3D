subroutine rdfour(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & nofou     ,kmax      ,lstsc     ,lsal      ,ltem      , &
                & gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
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
!    Function: - Read fourier input file, if available
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use properties, only: prop_get
    use string_module, only: remove_leading_spaces
    use system_utils, only: exifil
    use reafou_m, only: reafou
    !
    implicit none (type,external)
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: itis
!
! Global variables
!
    integer                   :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                   :: lsal   !  Description and declaration in dimens.igs
    integer                   :: lstsc  !  Description and declaration in dimens.igs
    integer                   :: ltem   !  Description and declaration in dimens.igs
    integer                   :: lundia !  Description and declaration in inout.igs
    integer                   :: lunmd  !  Description and declaration in inout.igs
    integer                   :: nofou  !  Description and declaration in dimens.igs
    integer                   :: nrrec  !!  Pointer to the record number in the MD-file
    logical                   :: error  !!  Flag=TRUE if an error is encountered
    character(*)              :: mdfrec !!  Standard rec. length in MD-file (300)
!
! Local variables
!
    integer                        :: lfile     ! Length of file name 
    integer                        :: lunfou    ! Unit number fourier input file 
    character(12)                  :: fildef    ! Default file name (usually = blank) 
    character(256)                 :: filfou    ! File name for fourier analysis input 
!
!! executable statements -------------------------------------------------------
!
    itis  => gdp%gdrdpara%itis
    !
    fildef = ' '
    filfou = fildef
    call prop_get(gdp%mdfile_ptr,'*','Filfou',filfou)
    !
    if (filfou/=fildef) then
       !
       !-------define length of file name
       !
       call remove_leading_spaces(filfou    ,lfile     )
       !
       !-------test file existence <YES>
       !
       if (exifil(filfou, lundia)) then
          !
          !---------read data from external file
          !
          open (newunit=lunfou, file = filfou(1:lfile), form = 'formatted', &
              & status = 'old')
          call reafou(error     ,lundia    ,lunfou    ,filfou    ,kmax      , &
                    & lstsc     ,lsal      ,ltem      ,nofou     ,gdp       )
          !
          close (lunfou)
       !
       !-------test file existence <NO>
       !
       else
          error = .true.
       endif
    endif
end subroutine rdfour
