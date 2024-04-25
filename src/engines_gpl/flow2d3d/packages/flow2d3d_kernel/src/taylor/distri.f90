subroutine distri(ttkiw     ,tiwtk     ,tkepro    ,tkedis    ,futg      , &
                & fvtg      ,fuiwe     ,fviwe     ,h0        ,thick     , &
                & bv2       ,ktop      ,kbed      ,kmax      ,kmxdt     , &
                & kmxt      ,luniwe    ,gdp       )
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
!    Function: Redistributes the TKE-IWE energy transfer rates
!              collected by routines TURIWE, SURIWE and BEDIWE
!              into TTKIW and TIWTK on the TG-grid to the hydro-
!              dynamic grid and assign these to TKEDIS and
!              TKEPRO, respectively.
!
!              TTKIW and TKEDIS are the TKE loss in [m^2/s^3] ;
!              TIWTK and TKEPRO are TKE production in [m^3/s^3].
!
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
    logical , pointer :: iwedia
!
! Global variables
!
    integer         :: kbed
                                   !!  K-value on TG grid: upper level
                                   !!  stratified layer
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: kmxdt !  Description and declaration in dimens.igs
    integer, intent(in)            :: kmxt !  Description and declaration in dimens.igs
    integer         :: ktop
                                   !!  K-value on TG grid: lower level
                                   !!  stratified layer
    integer, intent(in)            :: luniwe
                                   !!  Unit number for diagnostic reports of
                                   !!  this subprogram as well as called
                                   !!  subroutines
    real(fp), intent(in)               :: h0
                                   !!  Water depth
    real(fp), dimension(0:kmxdt) :: bv2
    real(fp), dimension(0:kmxdt) :: futg
    real(fp), dimension(0:kmxdt) :: fvtg
    real(fp), dimension(0:kmxdt), intent(in) :: tiwtk
    real(fp), dimension(0:kmxdt), intent(in) :: ttkiw
    real(fp), dimension(kmax) :: fuiwe !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax) :: fviwe !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax), intent(in) :: thick !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax) :: tkedis !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax) :: tkepro !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: k
    integer                        :: kdpm
    integer                        :: ktg
    real(fp)                       :: delz
    real(fp)                       :: dz
    real(fp)                       :: dzdpm
    real(fp)                       :: sumdis
    real(fp)                       :: sumpro
    real(fp)                       :: ztg
    real(fp)                       :: zuv
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    iwedia  => gdp%gdiwearr%iwedia
    !
    do kdpm = 1, kmax
       tkepro(kdpm) = 0.0
       tkedis(kdpm) = 0.0
    enddo
    !
    ! Integration over sigma-levels:
    !
    delz = 1./kmxt
    dz = h0*delz
    kdpm = 1
    zuv = 1. - thick(1) - 0.5*thick(2)
    !
    do ktg = 1, kmxt
       ztg = 1. - (ktg + 0.5)*delz
   40  continue
       if (ztg<zuv .and. kdpm<kmax) then
          dzdpm = h0*(thick(kdpm) + thick(kdpm + 1))/2.
          tkedis(kdpm) = tkedis(kdpm)/dzdpm
          tkepro(kdpm) = tkepro(kdpm)/dzdpm
          kdpm = min(kmax, kdpm + 1)
          zuv = zuv - thick(kdpm + 1)
          goto 40
       else
          tkedis(kdpm) = tkedis(kdpm) + ttkiw(ktg)*dz
          tkepro(kdpm) = tkepro(kdpm) + tiwtk(ktg)*dz
       endif
    enddo
    !
    ! Diagnostics:
    !
    sumpro = 0.0
    sumdis = 0.0
    do k = 1, kmax - 1
       dz = h0*(thick(k) + thick(k + 1))/2.
       sumpro = sumpro + tkepro(k)*dz
       sumdis = sumdis + tkedis(k)*dz
    enddo
    !
    if (iwedia) then
       write (luniwe, *)
       write (luniwe, '(a)') ' DISTRI                          :'
       write (luniwe, '(a,1x,g9.2)') ' Integral of TKE prod.  [m^3/s^3]:',      &
                                   & sumpro
       write (luniwe, '(a,1x,g9.2)') ' Integral of TKE diss.  [m^3/s^3]:',      &
                                   & sumdis
    endif
end subroutine distri
