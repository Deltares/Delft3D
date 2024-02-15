subroutine rdlaydis(error, thick, kmax, laydis, gdp)
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: lundia
!
! Global variables
!
    integer                      , intent(in)  :: kmax
    logical                                    :: error
    character(*)                 , intent(in)  :: laydis
    real(fp)    , dimension(kmax), intent(out) :: thick  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer        :: iter
    integer        :: itmax
    integer        :: k
    integer        :: ncol
    real(fp)       :: alfa
    real(fp)       :: alfb
    real(fp)       :: alfh
    real(fp)       :: alfn
    real(fp)       :: alft
    real(fp)       :: arg
    real(fp)       :: betta
    real(fp)       :: betta0
    real(fp)       :: dz
    real(fp)       :: dzbed
    real(fp)       :: dztop
    real(fp)       :: eps
    real(fp)       :: expbet
    real(fp)       :: gamlog
    real(fp)       :: gamma
    real(fp)       :: h0
    real(fp)       :: sig
    real(fp)       :: siglo
    real(fp)       :: sigup
    real(fp)       :: slope
    real(fp)       :: sum
    real(fp)       :: zlo
    real(fp)       :: zu
    real(fp)       :: zup
    real(fp)       :: zw
    character(256) :: message
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    !
    eps   = 1.0e-6_fp
    itmax = 100
    !
    h0 = 0.0_sp
    call prop_get(gdp%mdfile_ptr, '*', 'laydish0', h0)
    if (h0 == 0.0_sp) then
        message = 'Keyword laydish0 not found'
        call prterr(lundia, 'U021', trim(message))
        error = .true.
        goto 8888
    endif
    !
    dzbed = 0.0_sp
    call prop_get(gdp%mdfile_ptr, '*', 'laydisdzbed', dzbed)
    if (dzbed == 0.0_sp) then
        message = 'Keyword laydisdzbed not found'
        call prterr(lundia, 'U021', trim(message))
        error = .true.
        goto 8888
    endif
    !
    ! Checks:
    !
    if (laydis /= 'equid' .and. laydis /= 'power' .and.       &
      & laydis /= 'expon' .and. laydis /= 'expo2') then
       write (message, '(a,2x,a,1x,a)') 'Wrong text for type of layer distribution "', &
                                       & laydis, '" should be EQUID, POWER, EXPON or EXPO2.'
       call prterr(lundia, 'U021', trim(message))
       error = .true.
       goto 8888
    endif
    !
    ! Constant layer thickness:
    !
    if (laydis=='equid') then
       do k = 1, kmax
          thick(k) = 1.0_fp / kmax
       enddo
       write (lundia, '(a,2x,g13.6)') '* Constant layer thickness        THICK : ', &
                                    & thick(1)
    endif
    !
    ! Power-law distribution:
    !
    if (laydis=='power') then
       write (lundia, '(a)') '* Power-law distribution          THICK : '
       siglo = 1.0_fp
       alfa = dzbed
       do k = 1, kmax
          sigup = siglo
          siglo = (real(kmax - k,fp)/real(kmax,fp))**alfa
          thick(k) = sigup - siglo
       enddo
    endif
    !
    ! Exponential increase in layer thickness from bed upwards:
    !
    if (laydis=='expon') then
       !
       ! Check on applicability of exponential distribution:
       !
       gamma = kmax*dzbed/h0
       if (gamma > 1.0_fp) then
          write (message, '(2a)') 'Exponential z-distribution not required, ', &
                                & 'choose option EQUID.'
          call prterr(lundia, 'U021', trim(message))
          error = .true.
          goto 8888
       endif
       write (lundia, '(a)') '* Exponential distribution         THICK : '
       !
       ! Find power coefficient in one-sided exponential distribution:
       !
       iter   = 0
       itmax  = 100
       betta  = 1.0_fp
       gamma  = dzbed / h0
       gamlog = log(gamma)
       !
       ! Picard iteration:
       !
   40  continue
       iter   = iter + 1
       betta0 = betta
       betta  = abs(log(exp(betta0/kmax) - 1.0_fp + gamma) - gamlog)
       eps    = abs(betta - betta0)
       if (eps > 0.01_fp .and. iter <= itmax) goto 40
       if (eps > 0.01_fp) then
          write (message, '(2a)') 'No convergence in iteration for coefficient of ', &
                                & 'exponential layer distribution.'
          call prterr(lundia, 'U021', trim(message))
          error = .true.
          goto 8888
       endif
       !
       ! One-sided exponential distribution for given BETTA:
       !
       expbet = exp(betta) - 1.0_fp
       zlo    = 1.0_fp
       do k = 1, kmax
          zup      = zlo
          siglo    = real(kmax - k,fp) / real(kmax,fp)
          zlo      = (exp(betta*siglo) - 1.0_fp) / expbet
          thick(k) = zup - zlo
       enddo
    endif
    !
    ! Two-sided exponential distribution:
    !
    if (laydis == 'expo2') then
       dztop = 0.0_sp
       call prop_get(gdp%mdfile_ptr, '*', 'laydisdztop', dztop)
       if (dztop == 0.0_sp) then
           message = 'Keyword laydisdztop not found'
           call prterr(lundia, 'U021', trim(message))
           error = .true.
           goto 8888
       endif
       !
       ! Check on applicability of exponential distribution:
       !
       gamma = h0 / (real(kmax,fp)*dztop)
       if (gamma < 1.0_fp) then
          write (message, '(2a)') 'Top side of exponential z-distribution ', &
                                & 'not required, choose option EQUID.'
          call prterr(lundia, 'U021', trim(message))
          error = .true.
          goto 8888
       endif
       !
       gamma = h0 / (real(kmax,fp)*dzbed)
       if (gamma < 1.0_fp) then
          write (message, '(2a)') 'Bed side of exponential z-distribution ', &
                                & 'not required, choose option EQUID.'
          call prterr(lundia, 'U021', trim(message))
          error = .true.
          goto 8888
       endif
       !
       ! Find ALFT i.e. ALFA of top layer:
       !
       gamma = 2.0_fp * h0 / (real(kmax,fp)*dztop)
       iter  = 0
       itmax = 100
       alft  = 1.0_fp
       !
       ! Picard iteration:
       !
   60  continue
       iter = iter + 1
       alfn = alft
       alft = log(gamma*alfn + exp( - alfn))
       eps  = abs(alft - alfn)
       if (eps > 0.01_fp .and. iter <= itmax) goto 60
       if (eps > 0.01_fp) then
          write (message, '(2a)') 'No convergence in iteration for coefficient of ', &
                                & 'top-side of exponential layer distribution.'
          call prterr(lundia, 'U021', trim(message))
          error = .true.
          goto 8888
       endif
       !
       ! Find ALFB i.e. ALFA of bed layer:
       !
       gamma = 2.0_fp * h0 / (kmax*dzbed)
       iter  = 0
       itmax = 100
       alfb  = 1.0_fp
       !
       ! Picard iteration:
       !
   70  continue
       iter = iter + 1
       alfn = alfb
       alfb = log(gamma*alfn + exp( - alfn))
       eps  = abs(alfb - alfn)
       if (eps > 0.01_fp .and. iter <= itmax) goto 70
       if (eps > 0.01_fp) then
          write (message, '(2a)') 'No convergence in iteration for coefficient of ', &
                                & 'bed-side of exponential layer distribution.'
          call prterr(lundia, 'U021', trim(message))
          error = .true.
          goto 8888
       endif
       !
       ! Two-sided exponential distribution:
       !
       slope = alft - alfb
       zlo   = 1.0_fp
       do k = 1, kmax
          zup      = zlo
          sig      = real(kmax - k,fp) / real(kmax,fp)
          alfa     = alfb + slope*sig
          alfh     = 0.5_fp * alfa
          arg      = alfa * sig
          zlo      = 0.5_fp + tanh(arg - alfh)/(2.0_fp*tanh(alfh))
          thick(k) = zup - zlo
          if (k == kmax) thick(k) = zup
       enddo
    endif
    !
    ! Make sure the sum of layer thicknesses equals unity:
    !
    sum = 0.0_fp
    do k = 1, kmax
       sum = sum + thick(k)
    enddo
    !
    ! Redistribute:
    !
    do k = 1, kmax
       thick(k) = thick(k) / sum
    enddo
    !
    ! Dump for inspection and plotting:
    !
    write (lundia, '(a)') '***********************************************'
    write (lundia, '(a)') '* Vertical distribution u-grid:'
    write (lundia, '(a)') '*'
    write (lundia, '(a)') '* column 1: k number'
    write (lundia, '(a)') '* column 2: rel. thickness'
    write (lundia, '(a)') '* column 3: abs. thickness'
    write (lundia, '(a)') '* column 4: z-level cell centres'
    write (lundia, '(a)') '*'
    write (lundia, '(a)') 'ugrd'
    ncol = 4
    write (lundia, '(i3,1x,i2)') kmax, ncol
    !
    dz = h0 * thick(1)
    zu = h0 - 0.5_fp*dz
    k  = 1
    write (lundia, '(i3,3(2x,g13.6))') k, thick(k), dz, zu
    do k = 2, kmax
       zu = zu - 0.5_fp*dz
       dz = h0 * thick(k)
       zu = zu - 0.5_fp*dz
       write (lundia, '(i3,3(2x,g13.6))') k, thick(k), dz, zu
    enddo
    !
    write (lundia, '(a)') '***********************************************'
    write (lundia, '(a)') '* Vertical distribution w-grid:'
    write (lundia, '(a)') '*'
    write (lundia, '(a)') '* column 1: k number'
    write (lundia, '(a)') '* column 2: z-level interfaces relative to bed (z=0)'
    write (lundia, '(a)') '*'
    write (lundia, '(a)') 'wgrd'
    ncol = 2
    write (lundia, '(i3,1x,i2)') kmax + 1, ncol
    !
    k  = 0
    zw = h0
    write (lundia, '(i3,2(2x,g13.6))') k, zw
    do k = 1, kmax
       dz = h0 * thick(k)
       zw = zw - dz
       write (lundia, '(i3,2x,g13.6)') k, zw
    enddo
    !
    ! Sum must be 100
    !
    do k = 1, kmax
       thick(k) = thick(k) * 100.0_fp
    enddo
 8888 continue
end subroutine rdlaydis
