subroutine depfil(lundia    ,error     ,fildep    ,fmttmp    ,array     , &
                & nfld      ,ifld      ,dims      )
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
!!--description----------------------------------------------------------------- 
! 
!    Function: Reads the depth values from the attribute file 
! 
!!--pseudo code and references-------------------------------------------------- 
! NONE 
!!--declarations---------------------------------------------------------------- 
    use precision 
    use grid_dimens_module 
    use message_module
    use system_utils, only: exifil
    use quickin_dep_file
    use dfparall 
    ! 
    implicit none 
    ! 
    ! The following list of pointer parameters is used to point inside the dims structure 
    ! 
    integer, pointer                      :: mfg 
    integer, pointer                      :: mlg 
    integer, pointer                      :: nfg 
    integer, pointer                      :: nlg 
    integer, pointer                      :: mmaxgl 
    integer, pointer                      :: nmaxgl 
! 
! Global variables 
! 
    type(griddimtype), target                                          , intent(in)  :: dims   !  grid dimensions
    integer                                                            , intent(in)  :: ifld   !  index of field to be read
    integer                                                                          :: lundia !  unit number for diagnostic file
    integer                                                            , intent(in)  :: nfld   !  number of fields
    logical                                                            , intent(out) :: error  !  Flag=TRUE if an error is encountered 
    real(fp), dimension(nfld, dims%nlb:dims%nub, dims%mlb:dims%mub)    , intent(out) :: array  !  data array to fill
    character(*)                                                                     :: fildep !  Name of the relevant file 
    character(11)                                                      , intent(in)  :: fmttmp !  Format switch for the attribute file 
! 
! Local variables 
! 
    integer                               :: iocond ! Help variable for iostat condition  
    integer                               :: luntmp ! Unit number for attribute file  
    integer                               :: m 
    integer                               :: n 
    real(fp), dimension(:,:), allocatable :: dtmp   ! Temporary array containing dp of entire domain 
    character(300)                        :: errmsg ! Character string containing the error message to be written to file. The message depends on the error.
    !
    integer                               :: i
    integer                               :: nmaxio
    integer                               :: mmaxio
    integer , dimension(:,:), allocatable :: cnt
    real(hp), dimension(:,:), allocatable :: dtmpio
! 
!! executable statements ------------------------------------------------------- 
! 
    mfg    => dims%mfg 
    mlg    => dims%mlg 
    nfg    => dims%nfg 
    nlg    => dims%nlg 
    mmaxgl => dims%mmaxgl 
    nmaxgl => dims%nmaxgl 
    !
    error = .false.
    ! 
    ! Test file existence, if so read 
    ! 
    if (exifil(fildep, lundia)) then 
       ! 
       ! File exists 
       ! 
       ! allocate temporary array to store data of entire domain read from file 
       ! 
       ! NOTE: nmaxus and mmax equal nmaxgl and mmaxgl, respectively (for entire domain) 
       !       in case of parallel runs. Moreover, array is associated with subdomain and 
       !       therefore, data for entire domain is stored in temporary array dtmp 
       !
       allocate (dtmp(nmaxgl,mmaxgl)) 
       ! 
       ! the master opens and reads the depth file 
       ! 
       if ( inode /= master ) goto 10 
       ! 
       if (associated(dims%aggrtable)) then
          nmaxio = size(dims%aggrtable,1)
          mmaxio = size(dims%aggrtable,2)
          allocate (dtmpio(nmaxio,mmaxio),cnt(nmaxgl,mmaxgl))
          !
          call read_qndep(lundia    ,error     ,fildep    ,fmttmp    ,dtmpio    , &
                        & nmaxio    ,mmaxio    ,iocond    )
          cnt = 0
          dtmp = 0.0_fp
          do m = 1, mmaxio
             do n = 1, nmaxio
                i = dims%aggrtable(n,m)
                if (i>0) then
                   dtmp(i,1) = dtmp(i,1) + dtmpio(n,m)
                   cnt(i,1) = cnt(i,1) + 1
                endif
             enddo
          enddo
          do i = 1, nmaxgl
             if (cnt(i,1)>1) then
                dtmp(i,1) = dtmp(i,1)/cnt(i,1)
             endif
          enddo
          !
          deallocate(dtmpio,cnt)
       else
          call read_qndep(lundia    ,error     ,fildep    ,fmttmp    ,dtmp      , &
                        & nmaxgl    ,mmaxgl    ,iocond    )
       endif
       !
 10    continue 
       ! 
       ! check whether something went wrong with reading file 
       ! 
       call dfbroadc ( iocond, 1, dfint, error, errmsg )
       if ( error ) then
          !
          ! show error message when not yet done so
          !
          if (errmsg /= ' ') then
             call write_error(errmsg, unit=lundia)
          endif
       elseif (iocond /= 0) then 
          if (iocond < 0) then 
             errmsg = PREMATURE_EOF // trim(fildep)
             call write_error(errmsg, unit=lundia)
          else 
             errmsg = FILE_READ_ERROR // trim(fildep)
             call write_error(errmsg, unit=lundia)
          endif 
          error = .true. 
       else 
          ! 
          ! scatter array dtmp to all nodes 
          ! 
          call dfbroadc ( dtmp, nmaxgl*mmaxgl, dfloat, error, errmsg ) 
          if ( error ) then
             call write_error(errmsg, unit=lundia)
          else
             ! 
             ! put copies of parts of dtmp for each subdomain 
             ! 
             do m = mfg, mlg 
                do n = nfg, nlg 
                   array(ifld,n-nfg+1,m-mfg+1) = dtmp(n,m) 
                enddo 
             enddo 
          endif
       endif 
       ! 
       deallocate(dtmp) 
    else 
       ! 
       ! File does not exist 
       ! Exifil has produced a nice error message 
       !
       error = .true.
    endif 
end subroutine depfil 
