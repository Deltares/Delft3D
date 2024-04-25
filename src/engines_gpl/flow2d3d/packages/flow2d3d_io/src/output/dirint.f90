subroutine dirint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
                & funam     ,ntimwa    ,ntimwb    , &
                & atimw     ,btimw     ,func      ,fcom      ,dircos    , &
                & dirsin    ,hrmcom    ,gdp       )
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
!
! Method used:
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
!
! Global variables
!
    integer                                                                  , intent(in)  :: kmaxk  !!  Number of layers in the z-dir.
                                                                                                     !!  For values of func which need a third dimension, else 1
    integer                                                                                :: lundia !  Description and declaration in inout.igs
    integer                                                                  , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                  , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                                :: ntimwa !!  Time index of first function
    integer                                                                                :: ntimwb !!  Time index of second function
    integer      , dimension(2)                                              , intent(in)  :: ifcore !!  Time indices (cell id's) of the wave
                                                                                                     !!  functions which are in core available
    logical                                                                  , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                                 , intent(in)  :: atimw  !!  Interpolation factor for first function
    real(fp)                                                                 , intent(in)  :: btimw  !!  Interpolation factor for second function
    real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxk), intent(out) :: func   !!  Interpolated result
    real(fp)     , dimension(nmaxus, mmax, 2)                                              :: dircos !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nmaxus, mmax, 2)                                              :: dirsin !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nmaxus, mmax, kmaxk, 2)                                       :: fcom   !!  The two timesteps, defined by ifcore, of the function
    real(fp)     , dimension(nmaxus, mmax, kmaxk, 2)                         , intent(in)  :: hrmcom !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                           :: comfil !!  Name for communication file
                                                                                                     !!  com-<case><label>
    character(16)                                                                          :: funam  !!  Name of element which has to be read
    character(16)                                                                          :: grpnam !!  Data-group name defined for the COM-files (CURTIM)
!
!
! Local variables
!
    integer                        :: ierr   ! Flag for error when writing to Communication file 
    integer                        :: k
    integer                        :: m
    integer                        :: n
    integer        , external      :: neferr
    logical                        :: wrswch ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    real(fp)                       :: angle
    real(fp)                       :: fac
    character(256)                 :: errmsg ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----Initialize local variables
    !
    ierr = 0
    wrswch = .false.
    !
    !-----Check if the first required timestep is in core.
    !
    if (ntimwa/=ifcore(1) .and. ntimwa/=ifcore(2)) then
       !
       !--------Read the first timestep from file.
       !
       call getfield(ntimwa, comfil, grpnam, funam, ierr, fcom, nmaxus, mmax, kmaxk, 2, 1)
       if (ierr/=0) goto 9999
    elseif (ntimwa/=ifcore(1) .and. ntimwa==ifcore(2)) then
       !
       !--------Copy the second, in core available, time step to the first
       !        position.
       !
       do k = 1, kmaxk
          do m = 1, mmax
             do n = 1, nmaxus
                fcom(n, m, k, 1) = fcom(n, m, k, 2)
             enddo
          enddo
       enddo
    else
    endif
    !
    !-----Check if the second required timestep is in core.
    !
    if (ntimwb/=ifcore(2)) then
       !
       !---------Read the second timestep from file.
       !
       call getfield(ntimwa, comfil, grpnam, funam, ierr, fcom, nmaxus, mmax, kmaxk, 2, 2)
       if (ierr/=0) goto 9999
    endif
    !
    !-----Linear interpolation of wave directions
    !     Fcom contains wave directions in degrees
    !     this wave angle is splitted up in a cosine and a sine value based
    !     on the unity vector of 1. The weight of the vectors for
    !     interpolation is taken into account by the wave energy,
    !     simplified to Hrms**2
    !
    fac = atan(1.)*4./180.
    k = 1
    do m = 1, mmax
       do n = 1, nmaxus
          dircos(n, m, 1) = cos(fcom(n, m, k, 1)*fac)*hrmcom(n, m, k, 1)**2
          dircos(n, m, 2) = cos(fcom(n, m, k, 2)*fac)*hrmcom(n, m, k, 2)**2
          dirsin(n, m, 1) = sin(fcom(n, m, k, 1)*fac)*hrmcom(n, m, k, 1)**2
          dirsin(n, m, 2) = sin(fcom(n, m, k, 2)*fac)*hrmcom(n, m, k, 2)**2
       enddo
    enddo
    !-----Linear interpolation of the cosine values
    do m = 1, mmax
       do n = 1, nmaxus
          dircos(n, m, 1) = atimw*dircos(n, m, 1) + btimw*dircos(n, m, 2)
       enddo
    enddo
    !-----Linear interpolation of the sine values
    do m = 1, mmax
       do n = 1, nmaxus
          dirsin(n, m, 1) = atimw*dirsin(n, m, 1) + btimw*dirsin(n, m, 2)
       enddo
    enddo
    !-----func gets the value of the interpolated wave direction in degrees
    !-----at (0.,360.)
    do k = 1, kmaxk
       do m = 1, mmax
          do n = 1, nmaxus
             if (abs(dirsin(n,m,1))>0.0_fp .or. abs(dircos(n,m,1))>0.0_fp) then
                angle = atan2(dirsin(n, m, 1), dircos(n, m, 1))/fac
                if (angle<0.) angle = angle + 360.
             else
                angle = 0.0_fp
             endif
             func(n, m, k) = angle
          enddo
       enddo
    enddo
    !
 9999 continue
    if (ierr /= 0) then
       ierr = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine dirint
