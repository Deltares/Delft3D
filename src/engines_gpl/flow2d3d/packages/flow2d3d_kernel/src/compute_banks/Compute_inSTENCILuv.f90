subroutine Compute_inSTENCILuv(lunscr,Irov,mmax,nmax,nmaxus,kmax,nst,nlb,nub,mlb,mub,nmlb,nmub,zmodel,gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  Ndryact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:  defines inSTENCILu and inSTENCILv, that tells you if u or v are in the stencil for bilinear interpolation of IP
!               
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                , pointer :: totGHOSTu1
    integer                , pointer :: totGHOSTv1
    integer, dimension(:)  , pointer :: mIPu1
    integer, dimension(:)  , pointer :: nIPu1
    integer, dimension(:)  , pointer :: mIPv1
    integer, dimension(:)  , pointer :: nIPv1
    integer, dimension(:,:), pointer :: inSTENCILu
    integer, dimension(:,:), pointer :: inSTENCILv
    integer, dimension(:,:), pointer :: GHOSTu1
    integer, dimension(:,:), pointer :: GHOSTv1
!
! global variables
!
!
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    logical                                                             , intent(in)    :: Zmodel
!
!
!
! local variables
!
  integer                    :: I 
  integer                    :: nIPp1,nIPm1,nIP,mIP,mIPp1,mIPm1
!
!
! executable statements -------------------------------------------------------
!     
    totGHOSTu1 => gdp%gdimbound%totGHOSTu1
    totGHOSTv1 => gdp%gdimbound%totGHOSTv1
    mIPu1      => gdp%gdimbound%mIPu1
    nIPu1      => gdp%gdimbound%nIPu1
    mIPv1      => gdp%gdimbound%mIPv1
    nIPv1      => gdp%gdimbound%nIPv1
    inSTENCILu => gdp%gdimbound%inSTENCILu
    inSTENCILv => gdp%gdimbound%inSTENCILv
    GHOSTu1    => gdp%gdimbound%GHOSTu1
    GHOSTv1    => gdp%gdimbound%GHOSTv1
!  along psi velocity U1
! 
    inSTENCILu(1:nmaxus,1:mmax) = 0
!
    do i = 1,totGHOSTu1       
!
!      4 points of the stencil
       mIP = mIPu1(i)
       nIP = nIPu1(i) 
       mIPm1 = mIP-1
       nIPp1 = nIP+1
       ! define inSTENCILu
       if (GHOSTu1(nIP,mIP) == 0) then
          inSTENCILu(nIP,mIP)     = 1
       endif
       if (GHOSTu1(nIP,mIPm1) == 0) then
          inSTENCILu(nIP,mIPm1)   = 1
       endif
       if (GHOSTu1(nIPp1,mIP) == 0) then
          inSTENCILu(nIPp1,mIP)   = 1
       endif
       if (GHOSTu1(nIPp1,mIPm1) == 0) then
          inSTENCILu(nIPp1,mIPm1) = 1
       endif
!
    enddo
!
    inSTENCILv(1:nmaxus,1:mmax) = 0
!
    do i = 1,totGHOSTv1       
!
!      4 points of the stencil
       mIP = mIPv1(i)
       nIP = nIPv1(i) 
       mIPp1 = mIP+1
       nIPm1 = nIP-1
       ! define inSTENCILu
       if (GHOSTv1(nIP,mIP) == 0) then
          inSTENCILv(nIP,mIP)     = 1
       endif
       if (GHOSTv1(nIPm1,mIP) == 0) then
          inSTENCILv(nIPm1,mIP)   = 1
       endif
       if (GHOSTv1(nIP,mIPp1) == 0) then
          inSTENCILv(nIP,mIPp1)   = 1
       endif
       if (GHOSTv1(nIPm1,mIPp1) == 0) then
          inSTENCILv(nIPm1,mIPp1) = 1
       endif
    enddo
!
RETURN
end subroutine Compute_inSTENCILuv
