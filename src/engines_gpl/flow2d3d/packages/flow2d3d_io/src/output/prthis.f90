subroutine prthis(lundia    ,error     ,prshis    ,grdang    ,lunprt    , &
                & nuprpg    ,nuprln    ,header    ,iphisc    ,julday    , &
                & dtsec     ,nostat    ,ntruv     ,ntru      ,kmax      , &
                & lstsci    ,lsal      ,ltem      ,ltur      ,lmax      , &
                & namst     ,namtra    ,namcon    ,mnstat    ,mnit      , &
                & zwl       ,zalfas    ,zcuru     ,zcurv     ,zcurw     , &
                & zvicww    ,zdicww    ,zrich     ,zrho      ,gro       , &
                & ztur      ,ctr       ,fltr      ,atr       ,dtr       )
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
!    Function: Print the time varying history data
!              Selection is done using PRSHIS. For elements like
!              ZCURW where KMAX must be > 1 this coupling between
!              KMAX and PRSHIS is done in subroutine RDPRFL
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
!
! Global variables
!
    integer                                          , intent(in) :: iphisc !!  Current time counter for printing history data
    integer                                                       :: julday !  Description and declaration in inttim.igs
    integer                                          , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                          , intent(in) :: lmax   !  Description and declaration in dimens.igs
    integer                                                       :: lsal   !  Description and declaration in dimens.igs
    integer                                          , intent(in) :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: ltem   !  Description and declaration in dimens.igs
    integer                                          , intent(in) :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: lundia !  Description and declaration in inout.igs
    integer                                                       :: lunprt !  Description and declaration in inout.igs
    integer                                          , intent(in) :: nostat !  Description and declaration in dimens.igs
    integer                                                       :: ntru   !  Description and declaration in dimens.igs
    integer                                          , intent(in) :: ntruv  !  Description and declaration in dimens.igs
    integer        , dimension(2, nostat)            , intent(in) :: mnstat !  Description and declaration in stations.igs
    integer        , dimension(4, ntruv)             , intent(in) :: mnit   !  Description and declaration in stations.igs
    integer                                                       :: nuprln !  Description and declaration in postpr.igs
    integer                                                       :: nuprpg !  Description and declaration in postpr.igs
    logical                                                       :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                         , intent(in) :: dtsec  !!  Integration time step [in seconds]
    real(fp)                                         , intent(in) :: grdang !  Description and declaration in tricom.igs
    real(fp)       , dimension(nostat)               , intent(in) :: zalfas !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat)               , intent(in) :: zwl    !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, 0:kmax)       , intent(in) :: zdicww !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, 0:kmax)       , intent(in) :: zrich  !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, 0:kmax)       , intent(in) :: zvicww !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, 0:kmax, ltur) , intent(in) :: ztur   !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: zcuru  !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: zcurv  !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: zcurw  !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax)         , intent(in) :: zrho   !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(nostat, kmax, lstsci) , intent(in) :: gro    !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(ntruv)                , intent(in) :: ctr    !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(ntruv)                , intent(in) :: fltr   !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(ntruv, lstsci)        , intent(in) :: atr    !  Description and declaration in esm_alloc_real.f90
    real(fp)       , dimension(ntruv, lstsci)        , intent(in) :: dtr    !  Description and declaration in esm_alloc_real.f90
    character(131) , dimension(10)                                :: header !  Description and declaration in postpr.igs
    character(20)  , dimension(lmax)                              :: namcon !  Description and declaration in esm_alloc_char.f90
    character(20)  , dimension(nostat)               , intent(in) :: namst  !  Description and declaration in stations.igs
    character(20)  , dimension(ntruv)                , intent(in) :: namtra !  Description and declaration in stations.igs
    character(23)                                    , intent(in) :: prshis !  Description and declaration in tricom.igs
!
!
! Local variables
!
    integer                        :: i
    integer                        :: idate       ! Absolute date related to ITDATE and TIMSEC 
    integer                        :: iday         ! Number of days in idate 
    integer                        :: ihour        ! Number of hours in itime 
    integer                        :: imin         ! Number of minutes in itime 
    integer                        :: imon         ! Number of months in idate 
    integer                        :: isec         ! Number of seconds in itime 
    integer                        :: itime        ! Absolute time related to ITDATE and TIMSEC 
    integer                        :: iyear        ! Number of years in idate 
    integer                        :: k            ! Loop counter for KMAX 
    integer                        :: l            ! Loop counter for LSTSCI or LTUR 
    integer                        :: maxlin       ! Maximum number of lines in one page 
    integer                        :: n            ! Loop counter for NOSTAT or NTRUV 
    real(fp)                       :: adtr         ! Total mass transport (ATR + DTR) 
    real(fp)                       :: timmin       ! Time since ITDATE in minutes 
    real(fp)                       :: timsec       ! Time since ITDATE in seconds 
    real(fp)                       :: uvdir        ! Current direction 
    real(fp)                       :: uvmagn       ! Current magnitude 
    real(fp)                       :: zcurut       ! U-velocity in a defined station backwards transformed 
    real(fp)                       :: zcurvt       ! V-velocity in a defined station backwards transformed 
    character(20)                  :: adtsim       ! Absolute date and time related to ITDATE and TIMSEC 
    !
    data maxlin/67/
!
!! executable statements -------------------------------------------------------
!
    !
    !-----Define header data 'IPHISC'
    !
    timsec = iphisc*dtsec
    timmin = timsec/60.
    call timdat(julday    ,timsec    ,idate    ,itime     )
    iyear = idate/10000
    imon  = (idate - iyear*10000)/100
    iday  = idate - iyear*10000 - imon*100
    ihour = itime/10000
    imin  = (itime - ihour*10000)/100
    isec  = itime - ihour*10000 - imin*100
    write (adtsim, '(i4.4,2(a1,i2.2),a1,3(i2.2,a1) )') &
        & iyear, '-', imon, '-', iday, ' ', ihour, ':', imin, ':', isec, ' '
    !
    !-----element 'ZWL' only if PRSHIS( 1: 1) = 'Y'
    !
    if (prshis(1:1)=='Y') then
       !
       !--------Define header; test for new page and write header to print file
       !
       write (header(7), '(a,i6,a,f8.2,a,a10,a,a10,a)')                           &
             & ' Selected Water-levels at NST=', iphisc, ' ( =', timmin,          &
             & ' Elapsed min. ,Date : ', adtsim(:10), ' time :', adtsim(11:), ')'
       header(8) = '     M    N       Water-level   at station'
       header(9) = ' '
       if (nuprln + 6>=maxlin) then
          call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
       else
          write (lunprt, '(a)') header(6)
          write (lunprt, '(a)') header(7)
          write (lunprt, '(a)') header(8)
          write (lunprt, '(a)') header(9)
          write (lunprt, '(a)') header(10)
          nuprln = nuprln + 5
       endif
       !
       !--------Print Water-levels for all monitoring stations
       !
       do n = 1, nostat
          nuprln = nuprln + 1
          if (nuprln>=maxlin) call prhead(lunprt, header, nuprpg, nuprln)
          write (lunprt, '(1x,2(1x,i4),9x,f9.4,3x,a20)') &
              & mnstat(1, n), mnstat(2, n), zwl(n), namst(n)
       enddo
    endif
    !
    !-----element 'ZCURU', 'ZCURV', 'ZCURM' and 'ZCURD' last 2 in ZBUFF
    !     only if PRSHIS( 2: 5) <> 'NNNN'
    !
    if (index(prshis(2:5), 'Y')/=0) then
       !
       !--------define header; test for new page and write header to print file
       !
       write (header(7),'(a,i6,a,f8.2,a,a10,a,a10,a)')                                &
             & ' Selected Currents                  at NST=', iphisc, ' ( =', timmin, &
             & ' Elapsed min. ,Date : ', adtsim(:10), ' time :', adtsim(11:), ')'
       header(8) = '     M    N    K  Current in Zeta point' //                 &
                  & '                             at station'
       header(9) = '                    Magnitude   Direction' //               &
                  & '       U-KSI       V-ETA'
       if (nuprln + 6>=maxlin) then
          call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
       else
          write (lunprt, '(a)') header(6)
          write (lunprt, '(a)') header(7)
          write (lunprt, '(a)') header(8)
          write (lunprt, '(a)') header(9)
          write (lunprt, '(a)') header(10)
          nuprln = nuprln + 5
       endif
       !
       !--------Print Currents in zeta points for all monitoring stations
       !
       do k = 1, kmax
          do n = 1, nostat
             uvmagn = sqrt(zcuru(n, k)**2 + zcurv(n, k)**2)
             zcurut = zcuru(n, k)*cos(zalfas(n)*degrad) - zcurv(n, k)           &
                    & *sin(zalfas(n)*degrad)
             zcurvt = zcuru(n, k)*sin(zalfas(n)*degrad) + zcurv(n, k)           &
                    & *cos(zalfas(n)*degrad)
             if (zcuru(n, k)==0.0 .and. zcurv(n, k)==0.0) then
                uvdir = 0.
             else
                uvdir = 90. - atan2(zcurut, zcurvt)*raddeg + grdang
                uvdir = mod(uvdir + 2*360., 360.0_fp)
             endif
             nuprln = nuprln + 1
             if (nuprln>=maxlin) call prhead(lunprt, header, nuprpg, nuprln)
             write (lunprt, '(1x,3(1x,i4),1x,f12.4,f12.1,2f12.4,3x,a20)') &
                 & mnstat (1, n) , mnstat (2, n) , k, uvmagn , uvdir, zcuru (n, k)  &
                 & , zcurv (n, k) , namst (n)
          enddo
       enddo
    endif
    !
    !-----element 'ZCURW', only if KMAX > 1 (:= PRSHIS( 6: 6) = 'Y')
    !
    if (prshis(6:6)=='Y') then
       !
       !--------define header; test for new page and write header to print file
       !
       write (header(7),'(a,i6,a,f8.2,a,a10,a,a10,a)')                                &
             & ' Selected W-velocities              at NST=', iphisc, ' ( =', timmin, &
             & ' Elapsed min. ,Date : ', adtsim(:10), ' time :', adtsim(11:), ')'
       header(8) = '     M    N    K   W-velocity   at station'
       header(9) = ' '
       if (nuprln + 6>=maxlin) then
          call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
       else
          write (lunprt, '(a)') header(6)
          write (lunprt, '(a)') header(7)
          write (lunprt, '(a)') header(8)
          write (lunprt, '(a)') header(9)
          write (lunprt, '(a)') header(10)
          nuprln = nuprln + 5
       endif
       !
       !--------Print W-velocities for all monitoring stations
       !
       do k = 1, kmax
          do n = 1, nostat
             nuprln = nuprln + 1
             if (nuprln>=maxlin) call prhead(lunprt, header, nuprpg, nuprln)
             write (lunprt, '(1x,3(1x,i4),1x,1pe12.5,3x,a20)') &
                 & mnstat(1, n), mnstat(2, n), k, zcurw(n, k), namst(n)
          enddo
       enddo
    endif
    !
    !-----element 'GRO', only if LSTSCI > 0 (:= PRSHIS( 7:14) <> 'NNNNNNNN')
    !
    if (index(prshis(7:14), 'Y')/=0) then
       !
       !--------define header; test for new page and write header to print file
       !
       write (header(7),'(a,i6,a,f8.2, a,a10,a,a10,a)')                               &
             & ' Selected Concentrations            at NST=', iphisc, ' ( =', timmin, &
             & ' Elapsed min. ,Date : ', adtsim(:10), ' time :', adtsim(11:), ')'
       header(8) = '     M    N    K  Concentrations for L=1,LSTSCI'
       write (header(9),'(''                  at station          '',7(''           '',i2))') (i, i = 1, lstsci)
       if (nuprln + 6>=maxlin) then
          call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
       else
          write (lunprt, '(a)') header(6)
          write (lunprt, '(a)') header(7)
          write (lunprt, '(a)') header(8)
          write (lunprt, '(a)') header(9)
          write (lunprt, '(a)') header(10)
          nuprln = nuprln + 5
       endif
       !
       !--------Print concentrations for all monitoring stations
       !
       do k = 1, kmax
          do n = 1, nostat
             nuprln = nuprln + 1
             if (nuprln>=maxlin) call prhead(lunprt, header, nuprpg, nuprln)
             write (lunprt, '(1x,3(1x,i4),2x,a20,7(1x,1pe12.5))') &
                 & mnstat(1, n), mnstat(2, n), k, namst(n), (gro(n, k, l), l = 1, lstsci)
          enddo
       enddo
    endif
    !
    !-----element 'ZTUR', only if LTUR > 0 (:= PRSHIS(15:16) <> 'NN')
    !
    if (index(prshis(15:16), 'Y')/=0) then
       !
       !--------define header; test for new page and write header to print file
       !
       write (header(7),'(a,i6,a,f8.2,a,a10,a,a10,a)') &
             & ' Selected Turbulence quantities     at NST=', iphisc, ' ( =', timmin, &
             & ' Elapsed min. ,Date : ', adtsim(:10), ' time :', adtsim(11:), ')'
       header(8) = '     M    N    K  Turbulence quantities' // ' for L=1,LTUR'
       write (header(9),'(''                  at station          '',2(''           '',i2))') (l, l = 1, ltur)
       if (nuprln + 6>=maxlin) then
          call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
       else
          write (lunprt, '(a)') header(6)
          write (lunprt, '(a)') header(7)
          write (lunprt, '(a)') header(8)
          write (lunprt, '(a)') header(9)
          write (lunprt, '(a)') header(10)
          nuprln = nuprln + 5
       endif
       !
       !--------Print turbulence quantities for all monitoring stations
       !
       do k = 0, kmax
          do n = 1, nostat
             nuprln = nuprln + 1
             if (nuprln>=maxlin) call prhead(lunprt, header, nuprpg, nuprln)
             write (lunprt, '(1x,3(1x,i4),2x,a20,2(1x,1pe12.5))') &
                 & mnstat(1, n), mnstat(2, n), k, namst(n), (ztur(n, k, l) , l = 1, ltur)
          enddo
       enddo
    endif
    !
    !-----elements 'ZVICWW' and 'ZDICWW' only if KMAX > 1
    !     (:= PRSHIS(17:18) <> 'NN')
    !
    if (index(prshis(17:18), 'Y')/=0) then
       !
       !--------define header; test for new page and write header to print file
       !
       write (header(7),'(a,i6,a,f8.2,a,a10,a,a10,a)') &
             & ' Selected Vert.Eddy Visc/Diff &Rich at NST=', iphisc, ' ( =', timmin, &
             & ' Elapsed min. ,Date : ', adtsim(:10), ' time :', adtsim(11:), ')'
       header(8) = '     M    N    K   Vert. Eddy   Vert. Eddy' //              &
                  & '   Richardson   at station'
       header(9) = '                    Viscosity  Diffusivity' //              &
                  & '       Number'
       if (nuprln + 6>=maxlin) then
          call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
       else
          write (lunprt, '(a)') header(6)
          write (lunprt, '(a)') header(7)
          write (lunprt, '(a)') header(8)
          write (lunprt, '(a)') header(9)
          write (lunprt, '(a)') header(10)
          nuprln = nuprln + 5
       endif
       !
       !--------Print Vertical Eddy Viscosity and -Diffusivity for all
       !        monitoring stations
       !
       do k = 0, kmax
          do n = 1, nostat
             nuprln = nuprln + 1
             if (nuprln>=maxlin) call prhead(lunprt, header, nuprpg, nuprln)
             write (lunprt, '(1x,3(1x,i4),3(1x,1pe12.5),3x,a20)') &
                 & mnstat(1, n), mnstat(2, n), k, zvicww(n, k), zdicww(n, k), zrich(n, k),  &
                 & namst(n)
          enddo
       enddo
    endif
    !
    !-----element 'ZRHO', only if LSAL > 0 OR LTEM > 0
    !     (:= PRSHIS(19:19) = 'Y')
    !
    if (prshis(19:19)=='Y') then
       !
       !--------define header; test for new page and write header to print file
       !
       write (header(7),'(a,i6,a,f8.2,a,a10,a,a10,a)') &
             & ' Selected Density                   at NST=', iphisc, ' ( =', timmin, &
             & ' Elapsed min. ,Date : ', adtsim(:10), ' time :', adtsim(11:), ')'
       header(8) = '     M    N    K  Density       at station'
       header(9) = ' '
       if (nuprln + 6>=maxlin) then
          call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
       else
          write (lunprt, '(a)') header(6)
          write (lunprt, '(a)') header(7)
          write (lunprt, '(a)') header(8)
          write (lunprt, '(a)') header(9)
          write (lunprt, '(a)') header(10)
          nuprln = nuprln + 5
       endif
       !
       !--------Print Density for all monitoring stations
       !
       do k = 1, kmax
          do n = 1, nostat
             nuprln = nuprln + 1
             if (nuprln>=maxlin) call prhead(lunprt, header, nuprpg, nuprln)
             write (lunprt, '(1x,3(1x,i4),4x,f9.3,3x,a20)') &
                 & mnstat(1, n), mnstat(2, n), k, zrho(n, k), namst(n)
          enddo
       enddo
    endif
    !
    !-----element 'CTR', 'FLTR', 'ATR', 'DTR' and 'ADTR'
    !     only if PRSHIS(20:23),'Y') <> 'NNNN'
    !
    if (index(prshis(20:23), 'Y')/=0) then
       !
       !--------define header; test for new page and write header to print file
       !
       write (header(7),'(a,i6,a,f8.2,a,a10,a,a10,a)') &
             & ' Transports at Cross sections       at NST=', iphisc, ' ( =', timmin, &
             & ' Elapsed min. ,Date : ', adtsim(:10), ' time :', adtsim(11:), ')'
       header(8) = '    M1   N1   M2   N2 at cross-section  ' //                &
                  & ' current volume total volume     total a' //                &
                  & 'dvective total diffusive      total mass'
       header(9) = '                                        ' //                &
                  & '      transport     tranport L=  mass  t' //                &
                  & 'ransport mass  transport        tranport'
       if (nuprln + 6>=maxlin) then
          call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
       else
          write (lunprt, '(a)') header(6)
          write (lunprt, '(a)') header(7)
          write (lunprt, '(a)') header(8)
          write (lunprt, '(a)') header(9)
          write (lunprt, '(a)') header(10)
          nuprln = nuprln + 5
       endif
       !
       !--------Print Density for all monitoring stations
       !
       do n = 1, ntruv
          nuprln = nuprln + max(1, lstsci)
          if (nuprln>=maxlin) call prhead(lunprt, header, nuprpg, nuprln)
          if (lstsci>0) then
             adtr = atr(n, 1) + dtr(n, 1)
             write (lunprt,'(1x,4(1x,i4),1x,a20,2(1x,1pe12.5),(t70,i3,3(4x,1pe12.5)))') &
                   & mnit(1, n), mnit(2, n), mnit(3, n), mnit(4, n), namtra(n), ctr(n), &
                   & fltr(n), 1, atr(n, 1), dtr(n, 1), adtr
             do l = 2, lstsci
                adtr = atr(n, l) + dtr(n, l)
                write (lunprt, '(t70,i3,3(4x,1pe12.5))') &
                    & l, atr(n, l), dtr(n, l) , adtr
             enddo
          else
             write (lunprt, '(1x,4(1x,i4),1x,a20,2(1x,1pe12.5))') &
                 & mnit(1, n), mnit(2, n), mnit(3, n), mnit(4, n), namtra(n),  &
                 & ctr(n), fltr(n)
          endif
       enddo
    endif
    !
    write (lunprt, '(a)') header(10)
end subroutine prthis
