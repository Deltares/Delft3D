subroutine z_red_soursin(nmmax     ,kmax      ,thick     , &
                       & lsal      ,ltem      ,lsed      ,lsedtot , &
                       & dps       ,s0        ,s1        ,r0      , &
                       & nst       ,dzs1      ,kfsmax    , &
                       & kfsmin    ,kfs       ,gdp       )
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
!    Function: Reduces sourse and sink terms to avoid large
!              bed level changes
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sediment_basics_module
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , dimension(:,:)         , pointer :: kmxsed
    real(fp), dimension(:,:)         , pointer :: fixfac
    real(fp), dimension(:,:)         , pointer :: rsedeq
    real(fp), dimension(:,:)         , pointer :: sinkse
    real(fp), dimension(:,:)         , pointer :: sourse
    real(fp), dimension(:,:)         , pointer :: sour_im
    integer                          , pointer :: lundia
    integer                          , pointer :: ntstep
    real(fp)                         , pointer :: morfac
    real(fp)                         , pointer :: thresh
    real(fp)                         , pointer :: dzmax
    real(fp)                         , pointer :: bed
    integer                          , pointer :: itmor
    logical                          , pointer :: bedupd
    real(fp)                         , pointer :: hdt
    real(fp)      , dimension(:)     , pointer :: cdryb
    integer       , dimension(:)     , pointer :: tratyp
!
! Local parameters
!
    integer, parameter :: reducmessmax = 50
!
! Global variables
!
    integer                                                   , intent(in)  :: nmmax
    integer                                                   , intent(in)  :: kmax
    integer                                                   , intent(in)  :: lsal
    integer                                                   , intent(in)  :: ltem
    integer                                                   , intent(in)  :: lsed
    integer                                                   , intent(in)  :: lsedtot
    integer                                                   , intent(in)  :: nst
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin 
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmax 
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs1
    real(fp)  , dimension(kmax)                                             :: thick
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *)     , intent(in)  :: r0
!
! Local variables
!
    integer        :: k
    integer        :: klc
    integer        :: kmaxsd
    integer        :: l
    integer        :: ll
    integer        :: lstart
    integer        :: m
    integer        :: n
    integer        :: nm
    integer        :: reducmesscount
    real(fp)       :: dz
    real(fp)       :: h0
    real(fp)       :: h1
    real(fp)       :: reducfac
    real(fp)       :: thick0
    real(fp)       :: thick1
    character(150) :: message
!
!! executable statements -------------------------------------------------------
!
    kmxsed              => gdp%gderosed%kmxsed
    fixfac              => gdp%gderosed%fixfac
    rsedeq              => gdp%gderosed%rsedeq
    sinkse              => gdp%gderosed%sinkse
    sourse              => gdp%gderosed%sourse
    sour_im             => gdp%gderosed%sour_im
    lundia              => gdp%gdinout%lundia
    ntstep              => gdp%gdinttim%ntstep
    morfac              => gdp%gdmorpar%morfac
    thresh              => gdp%gdmorpar%thresh
    dzmax               => gdp%gdmorpar%dzmax
    bed                 => gdp%gdmorpar%bed
    itmor               => gdp%gdmorpar%itmor
    bedupd              => gdp%gdmorpar%bedupd
    hdt                 => gdp%gdnumeco%hdt
    cdryb               => gdp%gdsedpar%cdryb
    tratyp              => gdp%gdsedpar%tratyp
    !
    lstart         = max(lsal,ltem)
    reducmesscount = 0
    !
    do l = 1, lsedtot
       ll = lstart + l
       !
       ! Reduction is not applied to mud and not to bedl
       !
       if (tratyp(l) == TRA_COMBINE) then
          do nm = 1, nmmax
             if (kfs(nm) == 1) then
                !
                ! Apply reduction factor to source and sink terms if
                ! bottom is closer than user-specified threshold and
                ! erosive conditions are expected
                !
                kmaxsd = kmxsed (nm,l)
                h0     = max(0.01_fp, s0(nm) + real(dps(nm),fp))
                h1     = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                !
                thick   = 0.0_fp
                klc     = 1
                do k = kfsmax(nm),kfsmin(nm),-1
                   thick(klc) = dzs1(nm,k) / h1
                   klc        = klc + 1
                enddo
                !
                thick0 = thick(kmaxsd)*h0
                thick1 = thick(kmaxsd)*h1
                dz     = (hdt*morfac/cdryb(l)) &
                       & * (sourse(nm, l)*thick0 - (sinkse(nm, l)+sour_im(nm, l))*thick1*r0(nm, kmaxsd, ll))
                if (abs(dz) > h1*dzmax) then
                   reducfac = (h1*dzmax)/abs(dz)
                   if (reducfac < 0.01 .and. nst>=itmor .and. bedupd) then
                      !
                      ! Only write reduction warning when bed updating is true (and started)
                      ! (otherwise no problem)
                      ! Limit the number of messages with reducmessmax
                      ! (otherwise tri-diag will grow very fast)
                      !
                      reducmesscount = reducmesscount + 1
                      if (reducmesscount <= reducmessmax) then
                         call nm_to_n_and_m(nm, n, m, gdp)
                         write(message,'(a,i0,a,f12.2,a,3(i0,a))') &
                             & 'Source and sink term sediment ',l,' reduced with factor', &
                             & 1/reducfac,' (m,n)=(',m,',',n,'), after ',ntstep , ' timesteps.'
                         call prterr(lundia, 'Z013', trim(message))
                      endif
                    endif
                    sourse(nm, l)  = sourse(nm, l) *reducfac
                    sour_im(nm, l) = sour_im(nm, l)*reducfac
                    sinkse(nm, l)  = sinkse(nm, l) *reducfac
                endif
                !
                ! Apply reduction factor to source and sink
                ! terms if bottom is closer than user-specified
                ! threshold and erosive conditions are expected
                !
                !
                ! If erosion conditions are expected then apply
                ! reduction factor to sour and sink terms.
                ! estimate sink term based on previous cell
                ! concentration
                !
                if ((sinkse(nm,l)+sour_im(nm,l))*r0(nm,kmaxsd,ll) < sourse(nm,l)) then
                   sinkse(nm, l)  = sinkse(nm, l) *fixfac(nm,l)
                   sourse(nm, l)  = sourse(nm, l) *fixfac(nm,l)
                   sour_im(nm, l) = sour_im(nm, l)*fixfac(nm,l)
                   rsedeq(nm, l)  = rsedeq(nm, l) *fixfac(nm,l)
                endif
              endif           ! kfs
          enddo               ! nm
       endif                  ! tratyp
    enddo                     ! lsedtot
    if (reducmesscount > reducmessmax) then
       write (lundia,'(12x,a,i0,a)') 'Reduction messages skipped (more than ',reducmessmax,')'
       write (lundia,'(12x,2(a,i0))') 'Total number of reduction messages for timestep ', &
            & ntstep,' : ', reducmesscount
    endif
end subroutine z_red_soursin
