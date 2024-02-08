subroutine rdibch(lundia    ,error     ,runid     ,ntof      ,nto       , &
                & kc        ,kcd       ,omega     ,hydrbc    ,nhsub     , &
                & ntofgl    ,gdp       )
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
!    Function: - Reads the boundary condition records from the
!                intermediate TMP_//runid//.bch file
!              - The order of reading is sequential for each
!                opening.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use dfparall
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
    integer                         , intent(in)  :: kc     !  Description and declaration in dimens.igs
    integer                         , intent(in)  :: kcd    !  Description and declaration in dimens.igs
    integer                                       :: lundia !  Description and declaration in inout.igs
    integer                         , intent(in)  :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                         , intent(in)  :: ntof   !  Description and declaration in dimens.igs
    integer                         , intent(in)  :: ntofgl ! total number of H/A-type boundary sections of entire domain
    integer , dimension(ntof)       , intent(in)  :: nhsub  ! integer array to store sequence numbers of harmonic boundary condition in own subdomain
    logical                         , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp), dimension(4, nto, kcd), intent(out) :: hydrbc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kc)         , intent(out) :: omega  !  Description and declaration in esm_alloc_real.f90
    character(*)                                  :: runid  !!  Run identification code for the current simulation
!
! Local variables
!
    integer                               :: iocond
    integer                               :: k
    integer                               :: lrid    ! Length of character string RUNID 
    integer                               :: luntmp
    integer                               :: n
    integer, external                     :: newlun
    character(256)                        :: filbch
    real(fp), dimension(:,:), allocatable :: rtmp    ! temporary array containing hydrbc of entire domain
!
!! executable statements -------------------------------------------------------
!
    ! define length of runid
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    ! test file existence and if so read
    !
    filbch = 'TMP_' // runid(:lrid) // '.bch'
    !
    if (exifil(filbch, lundia)) then
       !
       ! allocate temporary array to store hydrbc of entire domain read from file
       !
       allocate (rtmp(ntofgl,kc))
       open (newunit=luntmp, file = filbch(:8 + lrid), form = 'unformatted', status = 'old')
       !
       ! unformatted file
       !
       !
       ! read number of frequencies (kc)
       !        this number already read in routine DIMRD
       !
       read (luntmp, iostat = iocond)
       !
       ! read kc frequencies (kc including mean value)
       !        hereafter read blank record
       !
       read (luntmp, iostat = iocond) (omega(k), k = 1, kc)
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filbch(:8 + lrid)    )
          else
             call prterr(lundia    ,'G007'    ,filbch(:8 + lrid)    )
          endif
          error = .true.
          goto 200
       endif
       !
       ! blank record
       !
       read (luntmp, iostat = iocond)
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filbch(:8 + lrid)    )
          else
             call prterr(lundia    ,'G007'    ,filbch(:8 + lrid)    )
          endif
          error = .true.
          goto 200
       endif
       !
       ! read per ntof kc amplitude for end A
       ! NOTE: ntof equals ntofgl (for entire domain) in case of parallel runs.
       !       Moreover, hydrbc is associated with subdomain and hence,
       !       hydrbc for entire domain is stored in temporary array rtmp
       !
       do n = 1, ntofgl
          read (luntmp, iostat = iocond) (rtmp(n, k), k = 1, kc)
          if (iocond/=0) then
             if (iocond<0) then
                call prterr(lundia    ,'G006'    ,filbch(:8 + lrid)    )
             else
                call prterr(lundia    ,'G007'    ,filbch(:8 + lrid)    )
             endif
             error = .true.
             goto 200
          endif
       enddo
       !
       ! put copies of parts of hydrbc for each subdomain
       !
       do n = 1, ntof
          hydrbc(1, n, 1:kc) = rtmp(nhsub(n), 1:kc)
       enddo
       !
       ! now the same for end B
       !
       do n = 1, ntofgl
          read (luntmp, iostat = iocond) (rtmp(n, k), k = 1, kc)
          if (iocond/=0) then
             if (iocond<0) then
                call prterr(lundia    ,'G006'    ,filbch(:8 + lrid)    )
             !
             else
                call prterr(lundia    ,'G007'    ,filbch(:8 + lrid)    )
             !
             endif
             error = .true.
             goto 200
          endif
       enddo
       !
       ! put copies of parts of hydrbc for each subdomain
       !
       do n = 1, ntof
          hydrbc(2, n, 1:kc) = rtmp(nhsub(n), 1:kc)
       enddo
       !
       ! blank record
       !
       read (luntmp, iostat = iocond)
       if (iocond/=0) then
          if (iocond<0) then
             call prterr(lundia    ,'G006'    ,filbch(:8 + lrid)    )
          else
             call prterr(lundia    ,'G007'    ,filbch(:8 + lrid)    )
          endif
          error = .true.
          goto 200
       endif
       !
       ! read per ntof kc-1 phases for end A
       ! kc = 1 is mean value, although no phases (in rdbch) it is
       ! written to the intermediate file
       ! NOTE: ntof equals ntofgl (for entire domain) in case of parallel runs.
       !       Moreover, hydrbc is associated with subdomain and hence,
       !       hydrbc for entire domain is stored in temporary array rtmp
       !
       do n = 1, ntofgl
          read (luntmp, iostat = iocond) (rtmp(n, k), k = 1, kc)
          if (iocond/=0) then
             if (iocond<0) then
                call prterr(lundia    ,'G006'    ,filbch(:8 + lrid)    )
             else
                call prterr(lundia    ,'G007'    ,filbch(:8 + lrid)    )
             endif
             error = .true.
             goto 200
          endif
       enddo
       !
       ! put copies of parts of hydrbc for each subdomain
       !
       do n = 1, ntof
          hydrbc(3, n, 1:kc) = rtmp(nhsub(n), 1:kc)
       enddo
       !
       ! now the same for end B
       !
       do n = 1, ntofgl
          read (luntmp, iostat = iocond) (rtmp(n, k), k = 1, kc)
          if (iocond/=0) then
             if (iocond<0) then
                call prterr(lundia    ,'G006'    ,filbch(:8 + lrid)    )
             else
                call prterr(lundia    ,'G007'    ,filbch(:8 + lrid)    )
             endif
             error = .true.
             exit
          endif
       enddo
       !
       ! put copies of parts of hydrbc for each subdomain
       !
       do n = 1, ntof
          hydrbc(4, n, 1:kc) = rtmp(nhsub(n), 1:kc)
       enddo
       !
       ! stop reading file
       !
       !
       ! close file
       !
  200  continue
       close (luntmp)
       deallocate(rtmp)
    !
    ! test file existence <NO>
    !
    else
       error = .true.
    endif
end subroutine rdibch
