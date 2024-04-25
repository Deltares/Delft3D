subroutine prtmap(lundia    ,error     ,prsmap    ,lunprt    ,nuprpg    , &
                & nuprln    ,header    ,ipmapc    ,julday    ,dtsec     , &
                & grdang    ,nmax      ,mmax      ,kmax      ,nmaxus    , &
                & lstsci    ,ltur      ,lmaxd     ,lsal      ,ltem      , &
                & namcon    ,kfu       ,kfv       ,kcs       ,s1        , &
                & u1        ,v1        ,wphy      ,alfas     ,r1        , &
                & rtur1     ,vicww     ,dicww     ,rich      ,rho       , &
                & rbuff0    ,rbuff1    ,velt      ,gdp       )
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
!    Function: Print the time varying map data
!              Selection is done using PRSMAP. For elements like
!              WPHY where KMAX must be > 1 this coupling between
!              KMAX and PRSMAP is done in subroutine RDPRFL
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
! Local parameters
!
    integer, parameter :: ncolp = 131
    integer, parameter :: nump = (ncolp - 5)/9
!
! Global variables
!
    integer                                                                    , intent(in)  :: ipmapc !!  Current time counter for printing map
                                                                                                       !!  data (called with IPMAP (NPMAPC))
    integer                                                                                  :: julday !  Description and declaration in inttim.igs
    integer                                                                    , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: lmaxd  !  Description and declaration in dimens.igs
    integer                                                                    , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer                                                                    , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                                                    , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: lundia !  Description and declaration in inout.igs
    integer                                                                                  :: lunprt !  Description and declaration in inout.igs
    integer                                                                    , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: nuprln !  Description and declaration in postpr.igs
    integer                                                                                  :: nuprpg !  Description and declaration in postpr.igs
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                             :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    logical                                                                                  :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                                   , intent(in)  :: dtsec  !!  Integration time step [in seconds]
    real(fp)                                                                                 :: grdang !  Description and declaration in tricom.igs
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                            :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: rich   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(in)  :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(in)  :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: rho    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(in)  :: wphy   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(in)  :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmaxus, mmax, 0:kmax, lmaxd)                                         :: rbuff0 !!  Help arrays for writing NEFIS files
    real(fp), dimension(nmaxus, mmax, 1:kmax, lmaxd)                                         :: rbuff1 !!  Help arrays for writing NEFIS files
    character(131), dimension(10)                                                            :: header !  Description and declaration in postpr.igs
    character(19)                                                              , intent(in)  :: prsmap !  Description and declaration in tricom.igs
    character(20), dimension(lstsci + ltur)                                    , intent(in)  :: namcon !  Description and declaration in esm_alloc_char.f90
    character(*)                                                               , intent(in)  :: velt   !  Velocity type 'eulerian' or 'GLM'
!
! Local variables
!
    integer       :: idate   ! Absolute date related to ITDATE and TIMSEC 
    integer       :: iday     ! Number of days in idate 
    integer       :: ihour    ! Number of hours in itime 
    integer       :: imin     ! Number of minutes in itime 
    integer       :: imon     ! Number of months in idate 
    integer       :: isec     ! Number of seconds in itime 
    integer       :: itime    ! Absolute time related to ITDATE and TIMSEC 
    integer       :: iyear    ! Number of years in idate 
    integer       :: k        ! Help var. 
    integer       :: kfs      ! Mask for the zeta points (time dependent) =0 inactive point =1 active   point 
    integer       :: l        ! Help var. 
    integer       :: m        ! Help var. 
    integer       :: maxlin   ! Maximum number of lines in one page 
    integer       :: md
    integer       :: n        ! Help var. 
    integer       :: nd
    integer       :: npr1
    integer       :: npr2
    real(fp)      :: timmin   ! Time since ITDATE in minutes 
    real(fp)      :: timsec   ! Time in seconds 
    character(20) :: adtsim   ! Absolute date and time related to ITDATE and TIMSEC 
    character(8)  :: eenh     ! Unit for concentrations depending on type of constituent 
    !
    data maxlin/67/
!
!! executable statements -------------------------------------------------------
!
    ! Define header data 'IPMAPC'
    !
    timsec = ipmapc*dtsec
    timmin = timsec/60.
    !
    call timdat(julday    ,timsec    ,idate    ,itime     )
    iyear = idate/10000
    imon  = (idate - iyear*10000)/100
    iday  = idate - iyear*10000 - imon*100
    ihour = itime/10000
    imin  = (itime - ihour*10000)/100
    isec  = itime - ihour*10000 - imin*100
    write (adtsim, '(i4.4,2(a1,i2.2),a1,3(i2.2,a1))') &
        & iyear, '-', imon, '-', iday, ' ', ihour, ':', imin, ':', isec, ' '
    !
    ! Define header
    !
    write (header(8),'(a,i6,a,f8.2,a,a10,a,a10,a)') &
        & '                                     at NST=',ipmapc,' ( =', timmin, &
        & ' Elapsed min. ,Date : ', adtsim(:10),' time :',adtsim(11:),')'
    !
    ! element 'S1' only if PRSMAP( 1: 1) = 'Y'
    !
    if (prsmap(1:1)=='Y') then
       !
       ! Define water-levels for active zeta points in CM
       !
       do m = 1, mmax
          md = max(1, m - 1)
          do n = 1, nmaxus
             nd = max(1, n - 1)
             kfs = min(1, (kfu(n, m) + kfu(n, md) + kfv(n, m) + kfv(nd, m)))
             rbuff1(n, m, 1, 1) = s1(n, m)*100.*kfs
          enddo
       enddo
       !
       ! Define text header
       !
       header(7) = ' Computed Water-levels (cm)         '
       !
       ! In sets of NUMP N indices
       !
       do npr1 = 1, nmaxus, nump
          npr2 = min(npr1 + nump - 1, nmaxus)
          !
          ! Test for new page and write HEADER to print file
          !
          write (header(9), '(''   N='',14i9)') (n, n = npr1, npr2)
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
          ! Print for all M indices
          !
          do m = 1, mmax
             nuprln = nuprln + 1
             !
             ! Test for maximum lines on page
             !
             if (nuprln>maxlin) then
                call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
             endif
             write (lunprt, '(1x,i3,1x,14f9.2)') m, (rbuff1(n, m, 1, 1), n = npr1, npr2)
          enddo
       enddo
    endif
    !
    ! element 'U1' only if PRSMAP( 2: 2) = 'Y'
    !
    if (prsmap(2:2)=='Y') then
       !
       ! Define U-KSI velocities in U-points in CM/SEC
       !
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                rbuff1(n, m, k, 1) = u1(n, m, k)*100.*kfu(n, m)
             enddo
          enddo
       enddo
       !
       ! For all layers
       !
       do k = 1, kmax
          !
          ! Define text header
          !
          write (header(7),'(3a,i4)') ' Computed U-KSI ', trim(velt), &
              & ' velocities (cm/sec),     layer number', k
          !
          ! In sets of NUMP N indices
          !
          do npr1 = 1, nmaxus, nump
             npr2 = min(npr1 + nump - 1, nmaxus)
             !
             ! Test for new page and write HEADER to print file
             !
             write (header(9), '(''   N='',14i9)') (n, n = npr1, npr2)
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
             ! Print for all M indices
             !
             do m = 1, mmax
                nuprln = nuprln + 1
                !
                ! Test for maximum lines on page
                !
                if (nuprln>maxlin) then
                   call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
                endif
                write (lunprt, '(1x,i3,1x,14f9.3)') m, (rbuff1(n, m, k, 1), n = npr1, npr2)
             enddo
          enddo
       enddo
    endif
    !
    ! element 'V1' only if PRSMAP( 3: 3) = 'Y'
    !
    if (prsmap(3:3)=='Y') then
       !
       ! Define V-ETA velocities in V-points in CM/SEC
       !
       do k = 1, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                rbuff1(n, m, k, 1) = v1(n, m, k)*100.*kfv(n, m)
             enddo
          enddo
       enddo
       !
       ! For all layers
       !
       do k = 1, kmax
          !
          ! Define text header
          !
          write (header(7),'(3a,i4)') ' Computed V-ETA ', trim(velt), &
              & ' velocities (cm/sec),     layer number', k
          !
          ! In sets of NUMP N indices
          !
          do npr1 = 1, nmaxus, nump
             npr2 = min(npr1 + nump - 1, nmaxus)
             !
             ! Test for new page and write HEADER to print file
             !
             write (header(9), '(''   N='',14i9)') (n, n = npr1, npr2)
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
             ! Print for all M indices
             !
             do m = 1, mmax
                nuprln = nuprln + 1
                !
                ! Test for maximum lines on page
                !
                if (nuprln>maxlin) then
                   call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
                endif
                write (lunprt, '(1x,i3,1x,14f9.3)') m, (rbuff1(n, m, k, 1), n = npr1, npr2)
             enddo
          enddo
       enddo
    endif
    !
    ! element 'WPHY' only if KMAX > 1 (:=  PRSMAP( 6: 6) = 'Y')
    !
    if (prsmap(6:6)=='Y') then
       !
       ! Define WPHY velocities in Zeta points in CM/SEC
       !
       do k = 1, kmax
          do m = 1, mmax
             md = max(1, m - 1)
             do n = 1, nmaxus
                nd = max(1, n - 1)
                kfs = min(1, (kfu(n, m) + kfu(n, md) + kfv(n, m) + kfv(nd, m)))
                rbuff1(n, m, k, 1) = wphy(n, m, k)*100.*kfs
             enddo
          enddo
       enddo
       !
       ! For all layers
       !
       do k = 1, kmax
          !
          ! Define text header
          !
          write (header(7),'('' Computed W-(phys.) velocities (cm/sec), layer number'',i4)') k
          !
          ! In sets of NUMP N indices
          !
          do npr1 = 1, nmaxus, nump
             npr2 = min(npr1 + nump - 1, nmaxus)
             !
             ! Test for new page and write HEADER to print file
             !
             write (header(9), '(''   N='',14i9)') (n, n = npr1, npr2)
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
             ! Print for all M indices
             !
             do m = 1, mmax
                nuprln = nuprln + 1
                !
                ! Test for maximum lines on page
                !
                if (nuprln>maxlin) then
                   call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
                endif
                write (lunprt, '(1x,i3,1x,14f9.5)') m, (rbuff1(n, m, k, 1), n = npr1, npr2)
             enddo
          enddo
       enddo
    endif
    !
    ! element 'R1', only if LSTSCI > 0 (:= PRSMAP( 7:14) <> 'NNNNNNNN')
    !
    if (index(prsmap(7:14), 'Y')/=0) then
       !
       ! Define Concentrations in Zeta-points in variable units
       !
       do l = 1, lstsci
          do k = 1, kmax
             do m = 1, mmax
                md = max(1, m - 1)
                do n = 1, nmaxus
                   nd = max(1, n - 1)
                   kfs = min(1, (kfu(n, m) + kfu(n, md) + kfv(n, m) + kfv(nd, m)&
                       & ))
                   rbuff1(n, m, k, l) = r1(n, m, k, l)*kfs
                enddo
             enddo
          enddo
       enddo
       !
       ! For all constituents, define units
       !
       do l = 1, lstsci
          eenh = '(kg/m3) '
          if (l==lsal) eenh = '(ppt)  '
          if (l==ltem) eenh = '(deg)  '
          !
          ! For all layers
          !
          do k = 1, kmax
             !
             ! Define text header
             !
             write (header(7),'('' Computed '',a,'' '',a,'', layer number'',i4)') &
                 & namcon(l), eenh, k
             !
             ! In sets of NUMP N indices
             !
             do npr1 = 1, nmaxus, nump
                npr2 = min(npr1 + nump - 1, nmaxus)
                !
                ! Test for new page and write HEADER to print file
                !
                write (header(9), '(''   N='',14i9)') (n, n = npr1, npr2)
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
                ! Print for all M indices
                !
                do m = 1, mmax
                   nuprln = nuprln + 1
                   !
                   ! Test for maximum lines on page
                   !
                   if (nuprln>maxlin) then
                      call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
                   endif
                   write (lunprt, '(1x,i3,1x,14f9.3)') m, (rbuff1(n, m, k, l), n = npr1, npr2)
                enddo
             enddo
          enddo
       enddo
    endif
    !
    ! element 'RTUR1', only if LTUR > 0 (:= PRSMAP(15:16) <> 'NN')
    !
    if (index(prsmap(15:16), 'Y')/=0) then
       !
       ! Define Turbulence quantities in Zeta-points
       !
       do l = 1, ltur
          do k = 0, kmax
             do m = 1, mmax
                md = max(1, m - 1)
                do n = 1, nmaxus
                   nd = max(1, n - 1)
                   kfs = min(1, (kfu(n, m) + kfu(n, md) + kfv(n, m) + kfv(nd, m)&
                       & ))
                   rbuff0(n, m, k, l) = rtur1(n, m, k, l)*10000.*kfs
                enddo
             enddo
          enddo
       enddo
       !
       ! For all turbulence quantities
       !
       do l = 1, ltur
          if (l==1) eenh = '(cm2/s3)'
          if (l==2) eenh = '(cm2/s2)'
          !
          ! For all layers
          !
          do k = 0, kmax
             !
             ! Define text header
             !
             write (header(7),'('' Computed '',a,'' '',a,'', layer number'',i4)') &
                 & namcon(lstsci + l), eenh, k
             !
             ! In sets of NUMP N indices
             !
             do npr1 = 1, nmaxus, nump
                npr2 = min(npr1 + nump - 1, nmaxus)
                !
                ! Test for new page and write HEADER to print file
                !
                write (header(9), '(''   N='',14i9)') (n, n = npr1, npr2)
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
                ! Print for all M indices
                !
                do m = 1, mmax
                   nuprln = nuprln + 1
                   !
                   ! Test for maximum lines on page
                   !
                   if (nuprln>maxlin) then
                      call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
                   endif
                   write (lunprt, '(1x,i3,1x,14f9.2)') m, (rbuff0(n, m, k, l), n = npr1, npr2)
                enddo
             enddo
          enddo
       enddo
    endif
    !
    ! element 'VICWW' if KMAX > 1 (:= PRSMAP(17:17) = 'Y')
    !
    if (prsmap(17:17)=='Y') then
       !
       ! Define Vertical eddy Viscosity in Zeta points in CM**2/SEC
       !
       do k = 0, kmax
          do m = 1, mmax
             md = max(1, m - 1)
             do n = 1, nmaxus
                nd = max(1, n - 1)
                kfs = min(1, (kfu(n, m) + kfu(n, md) + kfv(n, m) + kfv(nd, m)))
                rbuff0(n, m, k, 1) = vicww(n, m, k)*10000.*kfs
             enddo
          enddo
       enddo
       !
       ! For all layers
       !
       do k = 0, kmax
          !
          ! Define text header
          !
          write (header(7),'('' Computed Vert. Eddy Viscosity  (cm2/s), layer number'',i4)') k
          !
          ! In sets of NUMP N indices
          !
          do npr1 = 1, nmaxus, nump
             npr2 = min(npr1 + nump - 1, nmaxus)
             !
             ! Test for new page and write HEADER to print file
             !
             write (header(9), '(''   N='',14i9)') (n, n = npr1, npr2)
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
             ! Print for all M indices
             !
             do m = 1, mmax
                nuprln = nuprln + 1
                !
                ! Test for maximum lines on page
                !
                if (nuprln>maxlin) then
                   call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
                endif
                write (lunprt, '(1x,i3,1x,14f9.5)') m, (rbuff0(n, m, k, 1), n = npr1, npr2)
             enddo
          enddo
       enddo
    !
    endif
    !
    ! element 'DICWW' if KMAX > 1 (:= PRSMAP(18:18) = 'Y')
    !
    if (prsmap(18:18)=='Y') then
       !
       ! Define Vertical Eddy Diffusivity in Zeta points in CM**2/SEC
       !
       do k = 0, kmax
          do m = 1, mmax
             md = max(1, m - 1)
             do n = 1, nmaxus
                nd = max(1, n - 1)
                kfs = min(1, (kfu(n, m) + kfu(n, md) + kfv(n, m) + kfv(nd, m)))
                rbuff0(n, m, k, 1) = dicww(n, m, k)*10000.*kfs
             enddo
          enddo
       enddo
       !
       ! For all layers
       !
       do k = 1, kmax
          !
          ! Define text header
          !
          write (header(7),'('' Computed Vert.Eddy Diffusivity (cm2/s), layer number'',i4)') k
          !
          ! In sets of NUMP N indices
          !
          do npr1 = 1, nmaxus, nump
             npr2 = min(npr1 + nump - 1, nmaxus)
             !
             ! Test for new page and write HEADER to print file
             !
             write (header(9), '(''   N='',14i9)') (n, n = npr1, npr2)
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
             ! Print for all M indices
             !
             do m = 1, mmax
                nuprln = nuprln + 1
                !
                ! Test for maximum lines on page
                !
                if (nuprln>maxlin) then
                   call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
                endif
                write (lunprt, '(1x,i3,1x,14f9.5)') m, (rbuff0(n, m, k, 1), n = npr1, npr2)
             enddo
          enddo
       enddo
    !
    endif
    !
    ! element 'RICH' if KMAX > 1 (:= PRSMAP(17:18) <> 'NN')
    !
    if (index(prsmap(17:18), 'Y')>0) then
       !
       ! Define Richardson Numbers in zeta points
       !
       do k = 0, kmax
          do m = 1, mmax
             md = max(1, m - 1)
             do n = 1, nmaxus
                nd = max(1, n - 1)
                kfs = min(1, (kfu(n, m) + kfu(n, md) + kfv(n, m) + kfv(nd, m)))
                rbuff0(n, m, k, 1) = rich(n, m, k)*kfs
             enddo
          enddo
       enddo
       !
       ! For all layers
       !
       do k = 1, kmax
          !
          ! Define text header
          !
          write (header(7),'(a,i4)') ' Computed Richardson numbers(-), layer number', k
          !
          ! In sets of NUMP N indices
          !
          do npr1 = 1, nmaxus, nump
             npr2 = min(npr1 + nump - 1, nmaxus)
             !
             ! Test for new page and write HEADER to print file
             !
             write (header(9), '(''   N='',14i9)') (n, n = npr1, npr2)
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
             ! Print for all M indices
             !
             do m = 1, mmax
                nuprln = nuprln + 1
                !
                ! Test for maximum lines on page
                !
                if (nuprln>maxlin) then
                   call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
                endif
                write (lunprt, '(1x,i3,1x,14f9.5)') m, (rbuff0(n, m, k, 1), n = npr1, npr2)
             enddo
          enddo
       enddo
    !
    endif
    !
    ! element 'RHO' if LSAL > 0 or LTEM > 0 (:= PRSMAP(19:19) = 'Y')
    !
    if (prsmap(19:19)=='Y') then
       !
       ! Define Densities in Zeta points in KG/M**3
       !
       do k = 1, kmax
          do m = 1, mmax
             md = max(1, m - 1)
             do n = 1, nmaxus
                nd = max(1, n - 1)
                kfs = min(1, (kfu(n, m) + kfu(n, md) + kfv(n, m) + kfv(nd, m)))
                rbuff1(n, m, k, 1) = rho(n, m, k)*kfs
             enddo
          enddo
       enddo
       !
       ! For all layers
       !
       do k = 1, kmax
          !
          ! Define text header
          !
          write (header(7),'('' Computed Densities             (kg/m3), layer number'',i4)') k
          !
          ! In sets of NUMP N indices
          !
          do npr1 = 1, nmaxus, nump
             npr2 = min(npr1 + nump - 1, nmaxus)
             !
             ! Test for new page and write HEADER to print file
             !
             write (header(9), '(''   N='',14i9)') (n, n = npr1, npr2)
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
             ! Print for all M indices
             !
             do m = 1, mmax
                nuprln = nuprln + 1
                !
                ! Test for maximum lines on page
                !
                if (nuprln>maxlin) then
                   call prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
                endif
                write (lunprt, '(1x,i3,1x,14f9.2)') m, (rbuff1(n, m, k, 1), n = npr1, npr2)
             enddo
          enddo
       enddo
    !
    endif
end subroutine prtmap
