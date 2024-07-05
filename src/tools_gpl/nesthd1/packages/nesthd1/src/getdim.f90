subroutine getdim(lun    , mmax1  , nmax1 ,  mmax2 , nmax2 ,&
&maxnrp , maxbnd )
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
!***********************************************************************
! wl|Deltares         marine and coastal management
!
! subroutine         : getdim
! version            : v1.0
! date               : October 2000
! programmer         : Antoon Koster
!
! function           : Get dimensions as required for dynamical allocation
! limitations        :
! subroutines called : dimgrd - get dimensions for model grid
!                      dimbnd - get number of boundary segments
!***********************************************************************

   integer       lun(*)
   integer       mmax1, nmax1, mmax2, nmax2, maxnrp ,maxbnd
   character*80  line

   integer   lungrd1, lungrd2, lunbnd, lunadm

   lungrd1 = lun(1)
   lungrd2 = lun(3)
   lunbnd  = lun(5)
   lunadm  = lun(6)

!     reading dimensions for grid file overall model

   call dimgrd(lungrd1, lunadm, mmax1 , nmax1,' overall model ')
   mmax1 = mmax1 + 1
   nmax1 = nmax1 + 1

!     reading dimensions for grid file detail model

   call dimgrd(lungrd2, lunadm, mmax2 , nmax2,' detail model  ')
   mmax2 = mmax2 + 1
   nmax2 = nmax2 + 1

!     reading number of boundary segments

   call dimbnd(lunbnd,maxbnd )

!     maximum number of edges for grid enclosure

   maxnrp1  = nmax1*(mmax1 + 4)
   maxnrp2  = nmax2*(mmax2 + 4)
   maxnrp   = max(maxnrp1,maxnrp2)
   return
end




