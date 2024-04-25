subroutine fouini(lundia    ,lunfou    ,error     ,nofou     ,gdp       )
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
!    Function: - Checks if fourier analysis are requested
!                and detrmines the number of variables for
!                which a fourier analysis is requested
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, pointer :: nofouvar
!
! Local parameters
!
    integer, parameter :: maxvld = 40
!
! Global variables
!
    integer              :: lundia !  Description and declaration in inout.igs
    integer , intent(in) :: lunfou !!  Unit number for fourier input file
    integer              :: nofou  !  Description and declaration in dimens.igs
    logical              :: error  !!  Flag=TRUE if an error is encountered
!
! Local variables
!
    integer                            :: nveld  ! Used for format free reading 
    integer        , dimension(maxvld) :: il     ! Used for format free reading 
    integer        , dimension(maxvld) :: ir     ! Used for format free reading 
    character(300)                     :: record ! Used for format free reading 300 = 256 + a bit (field, =, ##, etc.) 
!
!! executable statements -------------------------------------------------------
!
    nofouvar => gdp%gdfourier%nofouvar
    ! initialisation
    !
    nofou    = 0
    nofouvar = 0
    !
    ! cycle through file, while reading records
    !
    ! -->
    !
   10 continue
    read (lunfou, '(a)', end = 999) record
    !
    ! reset record in smaller case characters and define contents
    !
    call small(record    ,300       )
    call regel(record    ,il        ,ir        ,maxvld    ,nveld     , &
             & error     )
    if (error) goto 999
    !
    ! test for continuation record
    !
    if (record(il(1):il(1))=='*' .or. nveld==0) goto 10
    !
    ! requested fourier analysis water-level
    !
    if (record(il(1):il(1) + 1)=='wl') then
       nofou = nofou + 1
       if (index(record,'min')>0) then
          nofouvar = nofouvar + 1
       else
          !
          ! max: also write max depth
          !
          nofouvar = nofouvar + 2
       endif
    !
    ! requested fourier analysis energy head
    !
    elseif (record(il(1):il(1) + 1)=='eh') then
       nofou = nofou + 1
       nofouvar = nofouvar + 1
    !
    ! requested fourier analysis velocity
    !
    elseif (record(il(1):il(1) + 1)=='uv') then
       nofou = nofou + 2
       if (index(record,'max')>0 .or. index(record,'min')>0) then
          nofouvar = nofouvar + 3
       elseif (index(record,'y')>0) then
          nofouvar = nofouvar + 8
       else
          nofouvar = nofouvar + 4
       endif
    !
    ! requested fourier analysis discharge
    !
    elseif (record(il(1):il(1) + 1)=='qf') then
       nofou = nofou + 2
       if (index(record,'max')>0 .or. index(record,'min')>0) then
          nofouvar = nofouvar + 3
       else
          nofouvar = nofouvar + 4
       endif
    !
    ! requested fourier analysis bedstress
    !
    elseif (record(il(1):il(1) + 1)=='bs') then
       nofou = nofou + 2
       if (index(record,'max')>0 .or. index(record,'min')>0) then
          nofouvar = nofouvar + 3
       else
          nofouvar = nofouvar + 4
       endif
    !
    ! requested fourier analysis temperature
    !
    elseif (record(il(1):il(1) + 1)=='ct') then
       nofou = nofou + 1
       if (index(record,'max')>0 .or. index(record,'min')>0) then
          nofouvar = nofouvar + 1
       else
          nofouvar = nofouvar + 2
       endif
    !
    ! requested fourier analysis salinity
    !
    elseif (record(il(1):il(1) + 1)=='cs') then
       nofou = nofou + 1
       if (index(record,'max')>0 .or. index(record,'min')>0) then
          nofouvar = nofouvar + 1
       else
          nofouvar = nofouvar + 2
       endif
    !
    ! requested fourier analysis constituent
    !
    elseif ((record(il(1):il(1))=='c') .and.                                    &
          & (record(il(1) + 1:il(1) + 1)=='1' .or. record(il(1) + 1:il(1) + 1)  &
           & =='2' .or. record(il(1) + 1:il(1) + 1)=='3' .or.                    &
          & record(il(1) + 1:il(1) + 1)=='4' .or. record(il(1) + 1:il(1) + 1)   &
           & =='5')) then
       nofou = nofou + 1
       if (index(record,'max')>0 .or. index(record,'min')>0) then
          nofouvar = nofouvar + 1
       else
          nofouvar = nofouvar + 2
       endif
    else
       !
       ! requested fourier analysis undefined
       !
       call prterr(lundia    ,'F001'    ,record(il(1):ir(1))  )
       !
       error = .true.
       goto 999
    endif
    !
    goto 10
    !
    ! <-- next record
    !
    !
  999 continue
end subroutine fouini
