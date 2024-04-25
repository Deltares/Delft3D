subroutine constwave(nmmax     ,dps       ,s0        ,alfas     ,ubot      , &
                  &  uorb      ,tp        ,teta      ,hrms      ,rlabda    , &
                  &  wlen      ,gdp       )
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
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: gammax
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: timmin
    logical                , pointer :: ubot_from_com
    logical                , pointer :: wlen_from_com
!
! Global variables
!
    integer                                     , intent(in)  :: nmmax
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: alfas
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: dps
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: hrms
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: rlabda
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: s0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: teta
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: tp
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: uorb
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: ubot
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: wlen
!
! Local variables
!
    integer                  :: nm
    integer                  :: nwav
    real(fp)                 :: hrms0
    real(fp)                 :: hw
    real(fp)                 :: incdir
    real(fp)                 :: k
    real(fp)                 :: k0
    real(fp)                 :: k0h
    real(fp)                 :: omega
    real(fp)                 :: per
    real(fp)                 :: thcart
    real(fp)                 :: tp0
    real(fp)                 :: uorb1
    real(fp), dimension(7)   :: wavcon
    integer , dimension(7)   :: isdir
    character(37)            :: wavnam
!
!! executable statements -------------------------------------------------------
!
    data isdir/0, 0, 1, 0, 0, 0, 1/
    !
    gammax          => gdp%gdnumeco%gammax
    ag              => gdp%gdphysco%ag
    timmin          => gdp%gdinttim%timmin
    ubot_from_com   => gdp%gdprocs%ubot_from_com
    wlen_from_com   => gdp%gdprocs%wlen_from_com
    !
    ! Initialize local variables
    !
    ! reading of wavecon file for time series of waves
    ! name is build using runid as extension
    !
    write(wavnam,'(a,a)') 'wavecon.', trim(gdp%runid)
    nwav        = 7
    call varcon(wavnam, timmin, wavcon, isdir, nwav, gdp)
    hrms0  = wavcon(1) / sqrt(2.0_fp)
    tp0    = wavcon(2)
    incdir = wavcon(3)
    do nm = 1, nmmax
       hw = max(0.01_fp, real(dps(nm),fp) + s0(nm))
       !
       ! Prevent unrealistic Hrms in shallow water
       !
       hrms(nm) = min(hrms0,gammax*hw)
       tp(nm)   = tp0
       per      = max(0.01_fp, tp(nm))
       omega    = 2.0_fp * pi / per
       k0       = omega * omega / ag
       k0h      = k0 * hw
       if (k0h > pi) then
          k = k0
       elseif (k0h < 0.005_fp) then
          k = omega/sqrt(ag*hw)
       else
          call wavenr(hw, per, k, ag)
       endif
       if (wlen_from_com) then
          rlabda(nm) = wlen(nm)
       else
          rlabda(nm) = 2.0_fp * pi / k
       endif
       if (ubot_from_com) then
          uorb(nm) = ubot(nm)
       else
          if (k*hw < 80.0_fp) then
             uorb1    = 0.5_fp * hrms(nm) * omega / sinh(k*hw)
             uorb(nm) = uorb1*sqrt(pi) / 2.0_fp
          else
             uorb(nm) = 0.0_fp
          endif
       endif
       thcart   = 270.0_fp - incdir
       teta(nm) = thcart - alfas(nm)
       if (teta(nm) > 360.0_fp) then
          teta(nm) = teta(nm) - 360.0_fp
       endif
    enddo
end subroutine constwave
