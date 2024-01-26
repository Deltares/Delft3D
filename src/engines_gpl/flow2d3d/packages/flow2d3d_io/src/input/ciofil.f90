subroutine ciofil(lundia    ,error     ,filcio    ,mmax      ,nmax      , & 
                & nmaxus    ,fcorio    ,gdp       ) 
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
!    Function: Reads the coriolis values from the attribute file 
! Method used: 
! 
!!--pseudo code and references-------------------------------------------------- 
! NONE 
!!--declarations---------------------------------------------------------------- 
    use precision 
    use dfparall 
    use globaldata 
    use system_utils, only: exifil
    ! 
    implicit none 
    ! 
    type(globdat),target :: gdp 
    ! 
    ! The following list of pointer parameters is used to point inside the gdp structure 
    ! 
! 
! Global variables 
! 
    integer                                                         , intent(in)  :: lundia     !  Description and declaration in inout.igs 
    integer                                                         , intent(in)  :: mmax       !  Description and declaration in esm_alloc_int.f90 
    integer                                                         , intent(in)  :: nmax       !  Description and declaration in esm_alloc_int.f90 
    integer                                                         , intent(in)  :: nmaxus     !  Description and declaration in esm_alloc_int.f90 
    logical                                                         , intent(out) :: error      !  Flag=TRUE if an error is encountered 
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(out) :: fcorio     !  Description and declaration in esm_alloc_real.f90 
    character(*)                                                    , intent(in)  :: filcio     !  Name of the relevant file 
! 
! 
! Local variables 
! 
    integer, pointer                      :: mfg 
    integer, pointer                      :: mlg 
    integer, pointer                      :: nfg 
    integer, pointer                      :: nlg 
    integer, pointer                      :: mmaxgl 
    integer, pointer                      :: nmaxgl 
    integer                               :: iocond   ! Help variable for iostat condition  
    integer                               :: lfile    ! Length of file name  
    integer                               :: luntmp   ! Unit number for attribute file  
    integer                               :: m        ! Help loop variable  
    integer                               :: n        ! Help loop variable 
    integer, external                     :: newlun 
    real(fp), dimension(:,:), allocatable :: ftmp     ! temporary array containing coriolis of entire domain 
    character(300)                        :: errmsg   ! Character string containing the errormessage to be written to file. The message depends on the error.  
! 
! 
!! executable statements ------------------------------------------------------- 
! 
    mfg    => gdp%gdparall%mfg 
    nfg    => gdp%gdparall%nfg 
    mlg    => gdp%gdparall%mlg 
    nlg    => gdp%gdparall%nlg 
    mmaxgl => gdp%gdparall%mmaxgl 
    nmaxgl => gdp%gdparall%nmaxgl 
    !
    ! Test file existence, if so read 
    ! 
    lfile = len(filcio) 
    ! 
    if (exifil(filcio, lundia)) then 
       ! 
       ! File exists 
       ! 
       ! allocate temporary array to store coriolis of entire domain read from file 
       ! 
       allocate (ftmp(nmaxgl,mmaxgl)) 
       ! 
       ! the master opens and reads the file 
       ! 
       if ( inode /= master ) goto 10 
       ! 
       open (newunit=luntmp, file = filcio(1:lfile), form = 'formatted', status = 'old') 
       ! 
       ! Freeformatted file, skip lines starting with a '*' 
       ! 
       call skipstarlines(luntmp    ) 
       ! 
       !--------read per NMAXUS, MMAX values in FCORIO array 
       !        NOTE: nmaxus and mmax equal nmaxgl and mmaxgl, respectively (for entire domain) 
       !              in case of parallel runs. Moreover, fcorio is associated with subdomain and 
       !              hence, fcorio for entire domain is stored in temporary array ftmp 
       !        end of file or error in file = not OK 
       ! 
       do n = 1, nmaxgl 
          read (luntmp, *, iostat = iocond) (ftmp(n, m), m = 1, mmaxgl) 
            if (iocond /= 0) then 
                if (iocond < 0) then 
                    call prterr(lundia, 'G006', trim(filcio))
                else 
                    call prterr(lundia, 'G007', trim(filcio))
                endif 
                error = .true. 
                goto 9999 
            endif 
            ! 
            ! If a NaN is read -> error
            ! 
            do m = 1, mmax 
                if ( isnan(ftmp(n, m)) ) then  
                    write(errmsg,'(2a)') 'NaN found in file ', trim(filcio) 
                    call prterr(lundia, 'P004', errmsg)
                    error = .true. 
                    goto 9999 
                endif 
            enddo 
       enddo 
       ! 
       ! Stop reading file 
       ! 
       9999  continue 
       ! 
       ! Close file 
       ! 
       close (luntmp) 
 10    continue 
       ! 
       ! check whether something went wrong with reading file 
       ! 
       call dfbroadc_gdp ( iocond, 1, dfint, gdp ) 
       ! 
       if (iocond/=0) then 
          if (iocond<0) then 
             call prterr(lundia    ,'G006'    ,filcio(1:lfile)      )
          ! 
          else 
             call prterr(lundia    ,'G007'    ,filcio(1:lfile)      )
          ! 
          endif 
          error = .true. 
       else 
          ! 
          ! scatter array ftmp to all nodes 
          ! 
          call dfbroadc_gdp ( ftmp, nmaxgl*mmaxgl, dfloat, gdp ) 
          ! 
          ! put copies of parts of fcorio for each subdomain 
          ! 
          do m = mfg, mlg 
             do n = nfg, nlg 
                fcorio(n-nfg+1,m-mfg+1) = ftmp(n,m) 
             enddo 
          enddo 
       endif 
       deallocate(ftmp) 
    else 
       ! 
       ! File does not exist 
       ! 
       error = .true. 
    endif 
end subroutine ciofil 
