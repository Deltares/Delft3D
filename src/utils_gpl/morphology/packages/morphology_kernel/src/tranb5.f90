subroutine tranb5(u         ,v         ,d50       ,d90       ,chezy     , &
                & h         ,hrms      ,tp        ,dir       ,npar      , &
                & par       ,dzdx      ,dzdy      ,vonkar    ,ws        , &
                & poros     ,sbotx     ,sboty     ,ssusx     ,ssusy     , &
                & cesus     )
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
! computes sediment transport according to
! bijker with wave effect
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
!
! Arguments
!
    integer                  , intent(in)    :: npar
    real(fp)                 , intent(in)    :: chezy
    real(fp)                 , intent(in)    :: d50
    real(fp)                 , intent(in)    :: d90
    real(fp)                 , intent(in)    :: dir
    real(fp)                 , intent(in)    :: dzdx
    real(fp)                 , intent(in)    :: dzdy
    real(fp)                 , intent(in)    :: h
    real(fp)                 , intent(in)    :: hrms
    real(fp), dimension(npar), intent(in)    :: par
    real(fp)                 , intent(in)    :: poros
    real(fp)                 , intent(in)    :: tp
    real(fp)                 , intent(in)    :: u
    real(fp)                 , intent(in)    :: v
    real(fp)                 , intent(in)    :: vonkar
    real(fp)                 , intent(in)    :: ws
    !
    real(fp)                 , intent(out)   :: sbotx
    real(fp)                 , intent(out)   :: sboty
    real(fp)                 , intent(out)   :: ssusx
    real(fp)                 , intent(out)   :: ssusy
    real(fp)                 , intent(out)   :: cesus
!
! Local variables
!
    integer                        :: ilun
    logical                        :: crstr
    logical                        :: exist
    logical, save                  :: first
    real(fp)                       :: ag                   ! gravity acceleration
    real(fp)                       :: arga
    real(fp)                       :: b                    ! correction coefficient shear stress
    real(fp)                       :: bd
    real(fp)                       :: bs
    real(fp)                       :: c90
    real(fp)                       :: cf
    real(fp)                       :: critd
    real(fp)                       :: crits
    real(fp)                       :: delta                ! relative density of sediment particle
    real(fp)                       :: eps                  ! converge criterium for
    real(fp)                       :: fac1
    real(fp)                       :: fac2
    real(fp)                       :: kw
    real(fp)                       :: por
    real(fp)                       :: relhrms
    real(fp)                       :: ri1
    real(fp)                       :: ri2
    real(fp)                       :: rk
    real(fp)                       :: rkh
    real(fp)                       :: rmu
    real(fp)                       :: sbeta
    real(fp)                       :: sbksi
    real(fp)                       :: sbota
    real(fp)                       :: sseta
    real(fp)                       :: ssksi
    real(fp)                       :: t                    ! continuity equation time in seconds
    real(fp)                       :: theta
    real(fp)                       :: uo                   ! orbital velocity
    real(fp)                       :: utot
    real(fp)                       :: uuvar                ! marginal depths in tidal flats
    real(fp)                       :: vstar
    real(fp)                       :: w                    ! flow velocity in z
    real(fp)                       :: z
    real(fp)                       :: zfact
    real(fp), external             :: fgyint
    real(hp), external             :: termfy
    real(hp), external             :: termgy
    real(fp)                       :: epssl
    real(fp)                       :: faca
    real(fp)                       :: facu
    !
    data first/.true./
!
!! executable statements -------------------------------------------------------
!
    if (first) then
       inquire (file = 'coef.inp', exist = exist)
       if (exist) then
          write (*, '(A)') 'Obsolete coef.inp file found; please use new keywords.'
          call throwexception()
       endif
       first = .false.
    endif
    !
    ag = par(1)
    delta = par(4)
    bs = par(11)
    bd = par(12)
    crits = par(13)
    critd = par(14)
    ! par(15) not used [was: d90]
    rk = par(16)
    if (rk < 0.0_fp) then
       rk = 12.0_fp * h * 10.0_fp ** -(chezy / 18.0_fp)
    endif
    w = par(17)
    if (w < 0.0_fp) then
       w = ws
    endif
    por = par(18)
    if (por < 0.0_fp) then
       por = poros
    endif
    if (tp < 1e-6_fp) then
       t = par(19)
    else
       t = tp
    endif
    faca = par(20)
    facu = par(21)
    epssl = par(22)
    crstr = epssl > 0.0_fp
    !
    if ((h/rk <= 1.33_fp) .or. (h > 200.0_fp)) then
       sbotx = 0.0_fp
       sboty = 0.0_fp
       ssusx = 0.0_fp
       ssusy = 0.0_fp
       cesus = 0.0_fp
       return
    endif
    !
    uuvar = 0.0_fp
    call wavenr(h         ,t         ,kw        ,ag        )
    theta = dir*degrad
    utot = sqrt(u*u + v*v)
    if (utot > 1.0e-10_fp) uuvar = utot*utot
    if (t > 1.0e-6_fp) call wave(uo, t, uuvar, pi, hrms, chezy, rk, h, ag, kw)
    cf = ag/chezy/chezy
    relhrms = hrms/h
    if (critd < relhrms .and. relhrms < crits) then
       fac1 = (bs - bd)/(crits - critd)
       fac2 = bd - fac1*critd
       b = fac2 + fac1*relhrms
    elseif (relhrms <= critd) then
       b = bd
    else
       b = bs
    endif
    c90 = 18.0_fp * log10(12.0_fp * h/d90)
    rmu = chezy/c90
    rmu = rmu*sqrt(rmu)
    if (uuvar > 1.0e-20_fp) then
       arga = -0.27_fp * delta * d50 * chezy*chezy/(rmu*uuvar)
       arga = max(arga, -50.0_fp)
       arga = min(arga, 50.0_fp)
       vstar = sqrt(cf)*sqrt(uuvar)
       z = w/vonkar/vstar
       z = min(z, 8.0_fp)
    else
       arga = -50.0_fp
       z = 8.0_fp
    endif
    sbota = b * d50/chezy * sqrt(ag) * exp(arga) * (1.0_fp - por)
    eps = 0.001_fp
    rkh = rk/h
    ri1 = 0.216_fp * rkh**(z - 1.0_fp)/(1.0_fp - rkh)**z * fgyint(rkh, 1.0_fp, z, eps, termfy)
    ri2 = 0.216_fp * rkh**(z - 1.0_fp)/(1.0_fp - rkh)**z * fgyint(rkh, 1.0_fp, z, eps, termgy)
    zfact = 1.83_fp
    cesus = zfact * sbota * (ri1*log(33.0_fp/rkh) + ri2)
    !
    if (crstr) then
       call bailtr(h         ,hrms      ,t         ,theta     ,w         , &
                 & dzdx      ,dzdy      ,sbksi     ,sbeta     ,ssksi     , &
                 & sseta     ,epssl     ,faca      ,facu      ,ag        )
    else
       sbksi = 0.0_fp
       sbeta = 0.0_fp
       ssksi = 0.0_fp
       sseta = 0.0_fp
    endif
    !
    if (utot > 1.0e-10_fp) then
       sbotx = sbota*u + sbksi + ssksi
       sboty = sbota*v + sbeta + sseta
       ssusx = cesus*u
       ssusy = cesus*v
    else
       sbotx = sbksi + ssksi
       ssusx = 0.0_fp
       sboty = sbeta + sseta
       ssusy = 0.0_fp
    endif
end subroutine tranb5
