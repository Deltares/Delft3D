subroutine morbndfill(kcs       ,guu       ,gvv       ,icx       ,icy       , &
                    & mnbnd     ,nto       ,gdp       )
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
!    Function: Fill arrays for open boundary
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: lundia
    type (bedbndtype)     , dimension(:) , pointer :: morbnd
!
! Global variables
!
    integer                                   , intent(in)  :: icx
    integer                                   , intent(in)  :: icy
    integer                                   , intent(in)  :: nto
    !
    integer , dimension(7, nto)               , intent(in)  :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    !
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: ddb
    integer  :: deltam
    integer  :: deltan
    integer  :: istat
    integer  :: i
    integer  :: ib
    integer  :: idir
    integer  :: idir2
    integer  :: jb
    integer  :: m
    integer  :: n
    integer  :: ndm
    integer  :: nf
    integer  :: nm
    integer  :: nmd
    integer  :: nmu
    integer  :: num
    integer  :: nmaxddb
    integer  :: npnt
    integer  :: nxmx
    integer  :: nxmx2
    !
    real(fp) :: totwidth
    real(fp) :: width
    real(fp) :: width2
    real(fp) :: widthprev
    real(fp) :: totdist
    !
    character(120) :: errmsg
!
!! executable statements -------------------------------------------------------
!
    lundia              => gdp%gdinout%lundia
    morbnd              => gdp%gdmorpar%morbnd
    !
    ddb     = gdp%d%ddbound
    nmaxddb = gdp%d%nmax+2*ddb
    !
    do jb = 1, nto
       !
       ! Prepare loop over boundary points
       !
       deltam = mnbnd(3,jb)-mnbnd(1,jb)
       deltan = mnbnd(4,jb)-mnbnd(2,jb)
       npnt   = max(abs(deltam),abs(deltan))+1
       deltam = (sign(1,deltam) - sign(1,-deltam))/2
       deltan = (sign(1,deltan) - sign(1,-deltan))/2
       !
       m  = mnbnd(1,jb)
       n  = mnbnd(2,jb)
       nm = (m+ddb-1)*nmaxddb + n + ddb
       !
       nmu = nm + icx
       num = nm + icy
       nmd = nm - icx
       ndm = nm - icy
       !
       ! Double the number of boundary points in case of a diagonal boundary
       !
       nf = 1
       if (      (kcs(nmu) == 1 .or. kcs(nmd) == 1) &
         & .and. (kcs(num) == 1 .or. kcs(ndm) == 1)  ) nf = 2
       !
       morbnd(jb)%npnt = nf*npnt
       istat           = 0
                       allocate(morbnd(jb)%alfa_dist(nf*npnt), stat = istat)
       if (istat == 0) allocate(morbnd(jb)%alfa_mag(nf*npnt) , stat = istat)
       if (istat == 0) allocate(morbnd(jb)%idir(nf*npnt)     , stat = istat)
       if (istat == 0) allocate(morbnd(jb)%nm(nf*npnt)       , stat = istat)
       if (istat == 0) allocate(morbnd(jb)%nxmx(nf*npnt)     , stat = istat)
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'MORBNDFIL: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       widthprev = 0.0
       totdist   = 0.0
       i         = 0
       do ib = 1, npnt
          i  = i + 1
          m  = mnbnd(1,jb)+(ib-1)*deltam
          n  = mnbnd(2,jb)+(ib-1)*deltan
          nm = (m+ddb-1)*nmaxddb + n + ddb
          !
          nmu = nm + icx
          num = nm + icy
          nmd = nm - icx
          ndm = nm - icy
          !
          ! Determine distance along grid lines or diagonals
          ! For diagonal boundaries +/- signs are defined with respect
          ! to the u/m direction. To get the transport direction correct
          ! in v/n direction, we need to add a minus sign if the direction
          ! of u/m and v/n directions are opposite.
          !
          if (abs(kcs(nmu)) == 1) then
             idir  = 1  ! u boundary
             nxmx  = nmu
             width = guu(nm)
             if (abs(kcs(num)) == 1) then
                nxmx2  = num
                idir2  = 2  ! v boundary
                width2 = gvv(nm)
             elseif (abs(kcs(ndm)) == 1) then
                nxmx2 = ndm
                idir2 = 2  ! v boundary
                !
                ! negative since v/n direction is reversed compared to u/m direction
                !
                width2 = -gvv(ndm)
             else
                width2 = 0.0
             endif
             totwidth = sqrt(width**2 + width2**2)
          elseif (abs(kcs(nmd)) == 1) then
             idir  = 1  ! u boundary
             nxmx  = nmd
             width = guu(nmd)
             if (abs(kcs(num)) == 1) then
                nxmx2 = num
                idir2 = 2  ! v boundary
                !
                ! negative since v/n direction is reversed compared to u/m direction
                !
                width2 = -gvv(nm)
             elseif (abs(kcs(ndm)) == 1) then
                nxmx2  = ndm
                idir2  = 2  ! v boundary
                width2 = gvv(ndm)
             else
                width2 = 0.0
             endif
             totwidth = sqrt(width**2 + width2**2)
          else
             idir = 2  ! v boundary
             if (abs(kcs(num)) == 1) then
                nxmx  = num
                width = gvv(nm)
             elseif (abs(kcs(ndm)) == 1) then
                nxmx  = ndm
                width = gvv(ndm)
             else
                write(errmsg,'(A,I0,A,I0,A)') 'MORBNDFIL: unable to determine orientation of mor boundary at M=',m,' N=',n,'.'
                call prterr(lundia, 'U021', errmsg)
                call d3stop(1, gdp)
             endif
             totwidth = width
          endif
          !
          ! Compute distance from "end A"
          !
          if (ib > 1) then
             totdist = totdist + 0.5*(totwidth + widthprev)
          endif
          widthprev = totwidth
          !
          ! Store data in data structure
          !
          morbnd(jb)%alfa_dist(i) = totdist
          morbnd(jb)%alfa_mag(i)  = 1.0
          morbnd(jb)%idir(i)      = idir
          morbnd(jb)%nm(i)        = nm
          morbnd(jb)%nxmx(i)      = nxmx
          !
          if (nf == 2) then
             morbnd(jb)%alfa_mag(i)  = width / totwidth
             i = i + 1
             morbnd(jb)%alfa_mag(i)  = width2 / totwidth
             morbnd(jb)%alfa_dist(i) = totdist
             morbnd(jb)%idir(i)      = idir2
             morbnd(jb)%nm(i)        = nm
             morbnd(jb)%nxmx(i)      = nxmx2
          endif
       enddo ! ib (boundary point)
       !
       if (npnt > 1) then
            do ib = 1, npnt
                if (abs(totdist)>0.0_fp) then
                    morbnd(jb)%alfa_dist(ib) = morbnd(jb)%alfa_dist(ib) / totdist
                else
                    morbnd(jb)%alfa_dist(ib) = 0.0_fp
                endif
            enddo
       endif
    enddo ! jb (open boundary)
end subroutine morbndfill
