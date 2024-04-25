module split_stations
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
!-------------------------------------------------------------------------------
private
public split_sta_parll
public get_sta_node

contains

    subroutine split_sta_parll(nostat, mnstat, namst, order_sta, lundia, gdp)
    use globaldata
    use dfparall, only: idir, ihalom, ihalon
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                    :: nostat    ! number of stations
    integer        , dimension(2,nostat)       :: mnstat    ! m and n indices of stations
    character(20)  , dimension(nostat)         :: namst
    integer        , dimension(:)    , pointer :: order_sta
    integer                                    :: lundia
    !
    integer                                    :: istat
    integer                                    :: m1
    integer                                    :: mfl          ! first m-index of this local partition, excluding the halo
    integer                                    :: mll          ! last  m-index of this local partition, excluding the halo
    integer                                    :: nfl          ! first n-index of this local partition, excluding the halo
    integer                                    :: nll          ! last  n-index of this local partition, excluding the halo
    integer                                    :: n
    integer                                    :: n1
    integer                                    :: nn
    integer      , dimension(:,:), allocatable :: itmp1        ! work array to store mnstat temporarily
    integer      , dimension(:)  , allocatable :: nsd          ! integer array to store sequence number of arrays for observation points in own subdomain
    character(20), dimension(:)  , allocatable :: ctemp        ! work array to store namst temporarily
    !
    if (nostat == 0) then
       !
       ! No observation points in the complete model:
       ! order_sta must be allocated with length 1 and value 0
       !
       allocate(order_sta(1), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
          call d3stop(1, gdp)
       endif
       order_sta(1) = 0
    else
       !
       ! nostat > 0
       !
       allocate(nsd(nostat), stat=istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
          call d3stop(1, gdp)
       endif
       nn  = 0
       nsd = 0
       if (idir == 1) then
          !
          ! n direction is split
          !
          mfl = 1
          mll = gdp%d%mmax
          if (gdp%gdparall%nfg == 1) then
             !
             ! first part; no halo in front of nfl
             !
             nfl = 1
          else
             !
             ! exclude halo in front of nfl
             !
             nfl = 1 + ihalon
          endif
          if (gdp%gdparall%nlg == gdp%gdparall%nmaxgl) then
             !
             ! last part; no halo behind nll
             !
             nll = gdp%d%nmaxus
          else
             !
             ! exclude halo behind nll
             !
             nll = gdp%d%nmaxus - ihalon
          endif
       elseif (idir == 2) then
          !
          ! m direction is split
          !
          nfl = 1
          nll = gdp%d%nmaxus
          if (gdp%gdparall%mfg == 1) then
             !
             ! first part; no halo in front of mfl
             !
             mfl = 1
          else
             !
             ! exclude halo in front of mfl
             !
             mfl = 1 + ihalom
          endif
          if (gdp%gdparall%mlg == gdp%gdparall%mmaxgl) then
             !
             ! last part; no halo behind mll
             !
             mll = gdp%d%mmax
          else
             !
             ! exclude halo behind mll
             !
             mll = gdp%d%mmax - ihalom
          endif
       endif
       do n = 1, nostat
          m1 = mnstat(1,n) - gdp%gdparall%mfg +1
          n1 = mnstat(2,n) - gdp%gdparall%nfg +1
          !
          ! check if observation point is inside or outside subdomain, excluding the halo
          !
          if ( m1>=mfl .and. n1>=nfl .and. m1<=mll .and. n1<=nll ) then
             !
             ! observation point is inside subdomain, store sequence number
             !
             mnstat(1,n) = m1
             mnstat(2,n) = n1
             nn          = nn +1
             nsd(nn)     = n
          endif
       enddo
       !
       ! restore mnstat and namst of own subdomain
       !
       ! in the parallel case, the original ordering of the
       ! stations is kept (for comparisons purpose) in order_sta
       ! order_sta is set to 0 when the partition does not contain
       ! any station
       if (nn == 0) then
          allocate(order_sta(1), stat=istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
             call d3stop(1, gdp)
          endif
          order_sta(1) = 0
       else
          istat = 0
          if (istat == 0) allocate(ctemp(nn)    , stat=istat)
          if (istat == 0) allocate(itmp1(2,nn)  , stat=istat)
          if (istat == 0) allocate(order_sta(nn), stat=istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'Rdsite: memory alloc error')
             call d3stop(1, gdp)
          endif
       endif
       do n = 1, nn
          order_sta(n) = nsd(n)
          ctemp(n)   = namst(nsd(n))
          itmp1(1,n) = mnstat(1,nsd(n))
          itmp1(2,n) = mnstat(2,nsd(n))
       enddo
       namst  = ' '
       mnstat = 0
       nostat = nn
       do n = 1, nostat
          namst(n)    = ctemp(n)
          mnstat(1,n) = itmp1(1,n)
          mnstat(2,n) = itmp1(2,n)
       enddo
       if (nn /= 0) deallocate(ctemp,itmp1, stat=istat)
       deallocate(nsd, stat=istat)
       !
       ! dummy values (nostat = 1) if final number found
       ! is 0 to avoid using a null ptr in subsequent
       ! routine calls
       !
       if (nn == 0) nostat = 1
       if (nostat == 1 .and. order_sta(1) == 0) then
          mnstat(1:2,1) = (/1,1/)
          namst(1) = ''
       endif
    endif
    end subroutine split_sta_parll

    subroutine get_sta_node(nostat, mnstat, inodest, gdp)
    use globaldata
    use dfparall, only: idir, ihalom, ihalon
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                                    :: nostat    ! number of stations
    integer        , dimension(2,nostat)       :: mnstat    ! m and n indices of stations
    integer        , dimension(nostat)         :: inodest
    !
    integer                                    :: m1
    integer                                    :: mfl          ! first m-index of this local partition, excluding the halo
    integer                                    :: mll          ! last  m-index of this local partition, excluding the halo
    integer                                    :: nfl          ! first n-index of this local partition, excluding the halo
    integer                                    :: nll          ! last  n-index of this local partition, excluding the halo
    integer                                    :: n
    integer                                    :: n1
    !
    if (nostat > 0) then
       if (idir == 1) then
          !
          ! n direction is split
          !
          mfl = 1
          mll = gdp%d%mmax
          if (gdp%gdparall%nfg == 1) then
             !
             ! first part; no halo in front of nfl
             !
             nfl = 1
          else
             !
             ! exclude halo in front of nfl
             !
             nfl = 1 + ihalon
          endif
          if (gdp%gdparall%nlg == gdp%gdparall%nmaxgl) then
             !
             ! last part; no halo behind nll
             !
             nll = gdp%d%nmaxus
          else
             !
             ! exclude halo behind nll
             !
             nll = gdp%d%nmaxus - ihalon
          endif
       elseif (idir == 2) then
          !
          ! m direction is split
          !
          nfl = 1
          nll = gdp%d%nmaxus
          if (gdp%gdparall%mfg == 1) then
             !
             ! first part; no halo in front of mfl
             !
             mfl = 1
          else
             !
             ! exclude halo in front of mfl
             !
             mfl = 1 + ihalom
          endif
          if (gdp%gdparall%mlg == gdp%gdparall%mmaxgl) then
             !
             ! last part; no halo behind mll
             !
             mll = gdp%d%mmax
          else
             !
             ! exclude halo behind mll
             !
             mll = gdp%d%mmax - ihalom
          endif
       endif
       do n = 1, nostat
          m1 = mnstat(1,n) - gdp%gdparall%mfg +1
          n1 = mnstat(2,n) - gdp%gdparall%nfg +1
          !
          ! check if observation point is inside or outside subdomain, excluding the halo
          !
          if ( m1>=mfl .and. n1>=nfl .and. m1<=mll .and. n1<=nll ) then
             !
             ! observation point is inside subdomain
             !
             mnstat(1,n) = m1
             mnstat(2,n) = n1
             inodest(n)  = 1
          else
             !
             ! observation point is outside subdomain
             !
             mnstat(1,n) = 0
             mnstat(2,n) = 0
             inodest(n)  = 0
          endif
       enddo
    endif
    end subroutine get_sta_node
end module split_stations