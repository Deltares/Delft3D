subroutine bndmorlyr(lsedtot   ,timhr     ,nto       , &
                   & bc_mor_array         ,gdp  )
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
!    Function: - Apply bed composition boundary conditions
!
!!--declarations----------------------------------------------------------------
    use precision
    use bedcomposition_module
    use flow_tables
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                             , pointer :: bed
    type (handletype)                    , pointer :: bcmfile
    type (bedbndtype), dimension(:)      , pointer :: morbnd
    type (cmpbndtype), dimension(:)      , pointer :: cmpbnd
    integer                              , pointer :: julday
!
! Global variables
!
    integer                       , intent(in) :: lsedtot
    integer                       , intent(in) :: nto
    real(fp)                                   :: timhr
    real(fp), dimension(2*lsedtot)             :: bc_mor_array
!
! Local variables
!
    integer  :: icond
    integer  :: ib
    integer  :: jb
    integer  :: l
    integer  :: nm
    integer  :: nxmx
    real(fp) :: alfa_dist
    real(fp) :: bndval
    real(fp) :: sedtot
    real(fp), dimension(lsedtot)         :: frac
!
!! executable statements -------------------------------------------------------
!
    bed                 => gdp%gdmorpar%bed
    bcmfile             => gdp%gdmorpar%bcmfile
    morbnd              => gdp%gdmorpar%morbnd
    cmpbnd              => gdp%gdmorpar%cmpbnd
    julday              => gdp%gdinttim%julday
    !
    do jb = 1, nto
       icond = cmpbnd(jb)%icond
       !
       ! If composition is fixed, nothing to do. So, we can
       ! continue with next boundary.
       !
       if (icond == 1) cycle
       !
       ! In case of an open boundary with prescribed composition
       ! (either mass or volume fractions): get data from table file
       !
       if (icond == 2 .or. icond == 3) then
          call flw_gettabledata(bcmfile     ,cmpbnd(jb)%ibcmt(1)    , &
                   & cmpbnd(jb)%ibcmt(2)    ,cmpbnd(jb)%ibcmt(3)    , &
                   & cmpbnd(jb)%ibcmt(4)    ,bc_mor_array           , &
                   & timhr      ,julday     ,gdp        )
          if (cmpbnd(jb)%ibcmt(3) == lsedtot) then
             do l = 1, lsedtot
                bc_mor_array(lsedtot + l) = bc_mor_array(l)
             enddo
          endif
       endif
       !
       ! Prepare loop over boundary points
       !
       do ib = 1, morbnd(jb)%npnt
          !
          alfa_dist = morbnd(jb)%alfa_dist(ib)
          nm        = morbnd(jb)%nm(ib)
          nxmx      = morbnd(jb)%nxmx(ib)
          !
          if (icond == 0) then
             !
             ! Free composition: copy composition from internal point
             !
             call copybedcomp(gdp%gdmorlyr, nxmx, nm)
          elseif (icond == 1) then
             !
             ! Fixed composition: no need to update the values
             !
          elseif (icond == 2) then
             !
             ! Prescribed mass fraction; needed volume fraction
             !
             do l = 1, lsedtot
                frac(l) = bc_mor_array(l) + &
                        & alfa_dist * (bc_mor_array(l+lsedtot)-bc_mor_array(l))
             enddo
             call setmfrac(gdp%gdmorlyr, frac, nm, nm)
          elseif (icond == 3) then
             !
             ! Prescribed volume fraction; needed volume fraction
             !
             do l = 1, lsedtot
                frac(l) = bc_mor_array(l) + &
                        & alfa_dist * (bc_mor_array(l+lsedtot)-bc_mor_array(l))
             enddo
             call setvfrac(gdp%gdmorlyr, frac, nm, nm)
          endif
       enddo
    enddo
end subroutine bndmorlyr
