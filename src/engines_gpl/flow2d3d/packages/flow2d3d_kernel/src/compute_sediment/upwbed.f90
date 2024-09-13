subroutine upwbed(su        ,sv        ,suu       ,svv       ,kfu       , &
                & kfv       ,kcs       ,kfsed     ,lsedtot   , &
                & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
                & nst       ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: upwbed.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/upwbed.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Copy transport rate from cell centres to velocity points
!              using an upwind or central approach.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sediment_basics_module
    use mathconsts, only: pi
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, dimension(:)                , pointer :: sedtyp
    integer                              , pointer :: islope
    real(fp)      , dimension(:)     , pointer :: sedd50
    real(fp)      , dimension(:)     , pointer :: sedd50fld
    real(fp)                             , pointer :: bed
    real(fp)                             , pointer :: alfabs
    real(fp)                             , pointer :: alfabn
    real(fp)                             , pointer :: ashld
    real(fp)                             , pointer :: bshld
    real(fp)                             , pointer :: cshld
    real(fp)                             , pointer :: dshld
    type (mornumericstype)               , pointer :: mornum
    logical               , pointer :: bdslpINupwnbed
    real(fp), dimension(:), pointer :: dzduuCENTR
    real(fp), dimension(:), pointer :: dzdvvCENTR
    real(fp)              , pointer :: ccofu_stored
!
! Global variables
!
    integer                                            , intent(in)  :: nst
    integer                                            , intent(in)  :: lsedtot
    integer                                            , intent(in)  :: icx
    integer                                            , intent(in)  :: icy
    integer                                            , intent(in)  :: nmmax
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfsed
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfu
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(inout)  :: su
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(inout)  :: sv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(in)  :: sutot
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(in)  :: svtot
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(out) :: suu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(out) :: svv
!
! Local variables
!
    integer  :: l
    integer  :: nm
    integer  :: nmu
    integer  :: num
    integer  :: nmd
    integer  :: ndm
    integer  :: ndmu
    integer  :: numd
    integer  :: numu
    logical  :: laterallyaveragedbedload
    logical  :: upwindbedload
    real(fp) :: suv1
    real(fp) :: suv2
    real(fp) :: sbedu
    real(fp) :: sbedv
    real(fp) :: sbedm
    real(fp) :: dzds
    real(fp) :: dzdn
    real(fp) :: dzdu
    real(fp) :: dzdv
    real(fp) :: phi
    real(fp) :: tphi
    real(fp) :: bagnol
    real(fp) :: alfas
    real(fp) :: di50
    real(fp) :: delta
    real(fp) :: ag
    real(fp) :: shield
    real(fp) :: ftheta
    real(fp) :: sina
    real(fp) :: cosa
    real(fp) :: tnorm
    logical  :: di50spatial
!
!! executable statements -------------------------------------------------------
!
    bdslpINupwnbed => gdp%gdimbound%bdslpINupwnbed
    dzduuCENTR     => gdp%gdimbound%dzduuCENTR
    dzdvvCENTR     => gdp%gdimbound%dzdvvCENTR
    ccofu_stored   => gdp%gdimbound%ccofu_stored
    sedtyp              => gdp%gdsedpar%sedtyp
    bed                 => gdp%gdmorpar%bed
    mornum              => gdp%gdmorpar%mornum
    islope              => gdp%gdmorpar%islope
    alfabs              => gdp%gdmorpar%alfabs
    alfabn              => gdp%gdmorpar%alfabn
    ashld               => gdp%gdmorpar%ashld
    bshld               => gdp%gdmorpar%bshld
    cshld               => gdp%gdmorpar%cshld
    dshld               => gdp%gdmorpar%dshld
    sedd50              => gdp%gdsedpar%sedd50
    sedd50fld           => gdp%gdsedpar%sedd50fld
    !
    upwindbedload            = mornum%upwindbedload
    laterallyaveragedbedload = mornum%laterallyaveragedbedload
    !
!   try to perform effect of bedslope here
!
    if (bdslpINupwnbed) then   
    do l = 1, lsedtot
       if (sedtyp(l) /= SEDTYP_COHESIVE) then
             di50        = sedd50(l)
             di50spatial = .false.
             if (di50<0 .and. lsedtot==1) di50spatial = .true.
          do nm = 1, nmmax
             !
                ! calculate magnitude of bed-load transport
                !
                dzdu = dzduuCENTR(nm)
                dzdv = dzdvvCENTR(nm)
                sbedu = su(nm, l)
                sbedv = sv(nm, l)
                sbedm    = sqrt(sbedu**2 + sbedv**2)
                !
                if (sbedm>0.00000001_fp) then
                   dzds =  dzdu*sbedu/sbedm + dzdv*sbedv/sbedm
                   dzdn = -dzdu*sbedv/sbedm + dzdv*sbedu/sbedm
                   !
                   ! limit dzds to 90% of phi
                   !
                   phi  = 30.0 / 180.0 * pi !take out cycle
                   tphi = tan(phi)          !take out cycle
                   dzds = min(0.9*tphi, dzds)
                   !
                   ! Apply bed slope effect according to
                   !   1: No correction
                   !   2: Bagnold (long. slope) and Ikeda / Van Rijn (transv. slope)
                   !   3: Van Bendegom and Koch & Flokstra
                   !   4: Parker and Andrews
                   !
                   select case (islope)
                   case(5) 
                      if (di50spatial) then
                        ! di50 = sqrt(sedd50fld(nm)) to be reintroduced
                      endif
                      delta   = 1.65 !(rhosol(l) - rhowat(nm,kbed))/rhowat(nm,kbed)
                      ag = 9.81
                      !
                      if (islope==5) then
                         !u_axis = 1.473450872804613
                         shield = 1.473450872804613_fp**2/di50/delta/ccofu_stored**2;
                         !if (comparereal(di50,0.0002_fp)==0) then
                         !   shield =    4.111851277592192_fp 
                         !elseif (comparereal(di50,0.001_fp)==0) then
                         !   shield =     0.822370255518438_fp 
                         !elseif (comparereal(di50,0.002_fp)==0) then
                         !   shield =     0.411185127759219_fp
                         !else
                         !   write(*,*) 'shield not defined for islope=5'
                         !   stop 
                         !   pause
                         !endif
                         ftheta  = ashld*(shield**bshld)  !note  cshld and dshld are zero by default,while ashld=0.85 and bshld=0.5 . So ftheta  = 0.85*(shield**0.5), as in eq 17 of Talmon 1995                           
                      endif
                      !
                      ! deal with exeptional case when ftheta, dzdv and dzdu are exactly
                      ! equal to zero
                      !
                      if (dzdu/=0.0_fp .or. dzdv/=0.0_fp) then
                         sina    = ftheta*sbedu/sbedm + dzdu
                         cosa    = ftheta*sbedv/sbedm + dzdv
                      else
                         sina    = sbedu/sbedm
                         cosa    = sbedv/sbedm
                      endif
                      tnorm = sqrt(sina**2 + cosa**2)
                      !
                      ! note adjusted bedload put in temporary array so doesn't influence
                      ! surrounding points
                      !
                      sbedm = sbedm * (1.0_fp + alfabs*dzds)
               
                      su(nm, l)  = sbedm * (sina/tnorm)
 
                      sv(nm, l)  = sbedm * (cosa/tnorm)

                      if (mod(nm-539,83)==0) then
                         write(777777,'(2i8,30f25.15)') nst,nm,sbedu,su(nm, l),sbedv,sv(nm, l),dzdu,dzdv
                      endif
                   case default
                      write(*,*) 'bdslpINupwnbed only allowed for islope=5'
                      !pause
                      stop
                   endselect
                endif           
             enddo
          endif
       enddo
    endif

    do l = 1, lsedtot
       if (sedtyp(l) /= SEDTYP_COHESIVE) then
          do nm = 1, nmmax
             !
             ! Try a scheme that reverts to central if transport directions oppose (G. Lesser) 
             !
             ! set bedload transport at u points
             !
             nmu = nm + icx
             !
             ! if active velocity point with two adjacent active sediment cells
             ! (done to prevent bed-load transport into kfsed=0 cells)
             !
             if ((kfu(nm)*kfsed(nm)*kfsed(nmu)) /= 0) then
                if (laterallyaveragedbedload) then
                   ndm = nm - icy
                   num = nm + icy
                   ndmu = nm + icx - icy
                   numu = nm + icx + icy
                   suv1 = (4.0 * su(nm, l) &
                         & + kfv(nm) * (su(num, l) - su(nm,l)) &
                         & + kfv(ndm) * (su(ndm, l) - su(nm,l)))/4.0
                   suv2 = (4.0*su(nmu,l) &
                         & + kfv(nmu) * (su(numu,l) - su(nmu,l)) &
                         & + kfv(ndmu) * (su(ndmu,l) - su(nmu,l)))/4.0
                else
                   suv1 = su(nm, l)
                   suv2 = su(nmu, l)
                endif
                if (kcs(nmu) == 3) then
                   !
                   ! correction for domain decomposition:
                   !
                   suu(nm, l) = suv1
                elseif (kcs(nm) == 3) then
                   suu(nm, l) = suv2
                elseif (sutot(nm, l)>0.0 .and. sutot(nmu, l)>0.0 .and. upwindbedload) then
                   suu(nm, l) = suv1
                elseif (sutot(nm, l)<0.0 .and. sutot(nmu, l)<0.0 .and. upwindbedload) then
                   suu(nm, l) = suv2
                else
                   suu(nm, l) = (suv1 + suv2)/2.0
                endif
             else
                suu(nm, l) = 0.0
             endif
             !
             ! set bedload transport at v points
             !
             num = nm + icy
             !
             ! if active velocity point with two adjacent active sediment cells
             ! (done to prevent bed-load transport into kfsed=0 cells)
             !
             if ((kfv(nm)*kfsed(nm)*kfsed(num)) /= 0) then
                if (laterallyaveragedbedload) then
                   nmd = nm - icx
                   nmu = nm + icx
                   numd = nm - icx + icy
                   numu = nm + icx + icy
                   suv1 = (4.0 * sv(nm, l) &
                         & + kfu(nm) * (sv(nmu, l) - sv(nm,l)) &
                         & + kfu(nmd) * (sv(nmd, l) - sv(nm,l)))/4.0
                   suv2 = (4.0*sv(num,l) &
                         & + kfu(num) * (sv(numu,l) - sv(num,l)) &
                         & + kfu(numd) * (sv(numd,l) - sv(num,l)))/4.0
                else
                   suv1 = sv(nm, l)
                   suv2 = sv(num, l)
                endif
                if (kcs(num) == 3) then
                   !
                   ! correction for domain decomposition:
                   !
                   svv(nm, l) = suv1
                elseif (kcs(nm) == 3) then
                   svv(nm, l) = suv2
                elseif (svtot(nm, l)>0.0 .and. svtot(num, l)>0.0 .and. upwindbedload) then
                   svv(nm, l) = suv1
                elseif (svtot(nm, l)<0.0 .and. svtot(num, l)<0.0 .and. upwindbedload) then
                   svv(nm, l) = suv2
                else
                   svv(nm, l) = (suv1 + suv2)/2.0
                endif
             else
                svv(nm, l) = 0.0
             endif
          enddo
       endif
    enddo
end subroutine upwbed
