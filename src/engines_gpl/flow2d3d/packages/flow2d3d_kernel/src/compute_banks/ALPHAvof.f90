subroutine ALPHAvof(n1,n2,n1pn2,V,alpha,INTx,INTy,INTwx,INTwy,L1,L2,Ndry,Nwet,edgeTYP,EDGlenDRY,EDGExyBANK)
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
!   Function:   given the normal (n1,n2), it computes the term alpha for the equation 
!               of the plane n1*x+n2*y=alpha. The two intersection points INTx,INTy are 
!               computed. The edge location L1 and L2 are returned. Type  of edge EDGEtyp
!               is also returned with values:
!              -2: dry vegetated edge
!               3: wet channel (non-vegetated) edge
!               0: cut edge
!               For the cut edge, the vector EDGlenDRY,EDGExyBANK return the length of the 
!               dry tract and the  coordinates of the two points delimiting the tract, respectively.
!               The first point of EDGExyBANK it is always part of the interface, while the second is a vertex of cell.
!               
!
!
!!--declarations----------------------------------------------------------------
!
  use precision
!
  implicit none
!
! global variables
!
  REAL(fp), intent(in)       :: V 
  REAL(fp), intent(in)       :: n1
  REAL(fp), intent(in)       :: n2   
  REAL(fp), intent(in)       :: n1pn2
  REAL(fp), intent(out)      :: alpha
  REAL(fp), intent(out)      :: INTx(5)  ! max number of wet points Ndry is 5
  REAL(fp), intent(out)      :: INTy(5)  ! max number of wet points Ndry is 5
  REAL(fp), intent(out)      :: INTwx(5) ! max number of wet points Ndry is 5
  REAL(fp), intent(out)      :: INTwy(5) ! max number of wet points Ndry is 5
  REAL(fp), intent(out)      :: EDGlenDRY(4)
  REAL(fp), intent(out)      :: EDGExyBANK(4,2,2)
  INTEGER,  intent(out)      :: L1
  INTEGER,  intent(out)      :: L2
  INTEGER,  intent(out)      :: Ndry
  INTEGER,  intent(out)      :: Nwet
  INTEGER,  intent(out)      :: edgeTYP(4)
  REAL(fp)                   :: V1
  REAL(fp)                   :: n
  REAL(fp)                   :: Vodd

!
! local variables
!
  integer                    :: I
  real(fp),parameter         :: oneSIXTH = 1._fp/6._fp
!
!
! executable statements -------------------------------------------------------
!    
    n=min(n1,n2)
    !n=n1
    !
    V1 = n/(2_fp*(1_fp - n))
    if (comparereal(V,V1).le.0) then  !I use comparereal because when V1 is 0 and V is zero i want to catch the equality, otherwise I get NaN when computing the intersection points below
       alpha = sqrt(2_fp*n*(1_fp - n)*V)
    elseif(V.lt.0.5_fp) then
       alpha = V*(1_fp-n)+0.5_fp*n
    elseif(V.lt.1_fp-V1) then ! note that 0<V1<0.5
       Vodd = 1_fp - V
       alpha = 1_fp - (Vodd*(1_fp-n)+0.5_fp*n)
    else 
       Vodd = 1_fp - V
       alpha = 1_fp - sqrt(2_fp*n*(1_fp - n)*Vodd)
    endif
    !
    ! alpha = alpha*(n1pn2)
    !   
    ! compute intersection point in reference configuration (Lx>0 and Ly>0) and create the vector of wet elements
    !
    if( ( comparereal(alpha,n1).lt.0 ).or. ( (comparereal(alpha,n1).eq.0 ) .and. ( comparereal(alpha,1._fp).eq.0) ) ) then !if both alpha and n1 are zero (i.e. n2=1, since n1+n2=1) , I want to catch the else option, that does not involve division by n1=zero). !if both alpha and n1 are 1 (i.e. n2=0, since n1+n2=1) I have to catch the first.
       edgeTYP(1) = 0 !CUTedge
       edgeTYP(2) =-2 !dry edge
       !dry polyon
       INTx(1) = alpha/n1
       if (INTx(1).gt.1._fp) then !I was having rounding problems (see below) I guess also this could happen
          INTx(1) = 1._fp
       endif
       INTy(1) = 0_fp
       INTx(2) = 1_fp
       INTy(2) = 0_fp
       INTx(3) = 1_fp
       INTy(3) = 1_fp
       L1 = 1
       Ndry = 3
       EDGlenDRY(1) = 1_fp-INTx(1) !IT is cheaper just to store EDGlenDRY in this subr instead of computing it as sqrt(dx**2+dy**2)
       EDGlenDRY(2) = 1_fp
       EDGExyBANK(1,1,1) = INTx(1) !x comp of first (not-nodal) point in cut edge
       EDGExyBANK(1,1,2) = INTy(1) !y comp of first (not-nodal) point in cut edge
       EDGExyBANK(1,2,1) = 1_fp !x comp of second  point in cut edge
       EDGExyBANK(1,2,2) = 0_fp !y comp of second  point in cut edge
       EDGExyBANK(2,1,1) = 1_fp !x comp of first  node
       EDGExyBANK(2,1,2) = 0_fp !y comp of first  node
       EDGExyBANK(2,2,1) = 1_fp !x comp of second node
       EDGExyBANK(2,2,2) = 1_fp !y comp of second node
       !wet polygon
       INTwx(1) = INTx(1)
       INTwy(1) = 0._fp
       INTwx(2) = 0._fp
       INTwy(2) = 0._fp
       Nwet = 2
!
    else
       !
       ! dry polygon
       !
       INTx(1) = 1_fp
       INTy(1) =  alpha/n2-n1/n2 !(alpha/n1-1.d0)*n1/n2
       if (INTy(1).lt.0._fp) then ! I was having rounding problems below, intx(Ndry)= 58.9545661603361	-58.954566160336= -5.684341886080801D-014	NEGATIVE!
          INTy(1) = 0._fp
       elseif (INTy(1).gt.1._fp) then !I guess also this could happen
          INTy(1) = 1._fp
       endif
       INTx(2) = 1_fp
       INTy(2) = 1_fp
       L1 = 2
       Ndry = 2
       EDGlenDRY(1) = 0_fp
       EDGlenDRY(2) = 1_fp-INTy(1)
       EDGExyBANK(1,1,1) = 0_fp !x comp of first  node
       EDGExyBANK(1,1,2) = 0_fp !y comp of first  node
       EDGExyBANK(1,2,1) = 1_fp !x comp of second node
       EDGExyBANK(1,2,2) = 0_fp !y comp of second node
       EDGExyBANK(2,1,1) = 1_fp !x comp of first (not-nodal) point in cut edge 
       EDGExyBANK(2,1,2) = INTy(1) !y comp of first (not-nodal) point in cut edge 
       EDGExyBANK(2,2,1) = 1_fp !x comp of second point in cut edge 
       EDGExyBANK(2,2,2) = 1_fp !y comp of second point in cut edge 
       edgeTYP(1) = 3 !wet edge
       if (comparereal(EDGlenDRY(2),0.999999999999_fp).lt.0) then !  it was    if (comparereal(EDGlenDRY(2),1._fp).ne.0) then  :changed only to make testcase X16 freeSLIP=0 with glitch run
          edgeTYP(2) = 0 !cut edge
       else ! in this case I have the dry region exacting intersecting the vertex, so it is not a cut edge but a  dry one
          edgeTYP(2) = -2 !dry edge
       endif
       !wet polygon
       INTwx(1) = 1._fp 
       INTwy(1) = INTy(1)
       INTwx(2) = 1._fp
       INTwy(2) = 0._fp
       INTwx(3) = 0._fp
       INTwy(3) = 0._fp
       Nwet = 3
    endif
    if ( (comparereal(alpha,n2).lt.0 ) .or. ( (comparereal(alpha,n2).eq.0 ) .and. ( comparereal(alpha,1._fp).eq.0) ) ) then !here instead if both alpha and n2 are 0 (i.e. n1=1),  I want to catch the second option, that does not involve division by n2=zero). If both alpha and n2 are 1 (i.e. n1=0),and if alpha=0 and n2=1 (i.e. n1=0) I want to catch the first
       edgeTYP(3) =-2 !dry edge
       edgeTYP(4) = 0 !cut edge
       Ndry=Ndry+1
       INTx(Ndry) = 0_fp
       INTy(Ndry) = 1_fp
       Ndry=Ndry+1
       INTx(Ndry) = 0_fp
       INTy(Ndry) = alpha/n2
       if (INTy(Ndry).gt.1._fp) then !I was having rounding problems (see below) I guess also this could happen
          INTy(Ndry) = 1._fp
       endif
       EDGlenDRY(3) = 1_fp
       EDGlenDRY(4) = 1_fp-INTy(Ndry)
       L2 = 4
       EDGExyBANK(3,1,1) = 1_fp !x comp of first  node
       EDGExyBANK(3,1,2) = 1_fp !y comp of first  node
       EDGExyBANK(3,2,1) = 0_fp !x comp of second node
       EDGExyBANK(3,2,2) = 1_fp !y comp of second node
       EDGExyBANK(4,1,1) = INTx(Ndry) !x comp of first (not-nodal) point in cut edge 
       EDGExyBANK(4,1,2) = INTy(Ndry) !y comp of first (not-nodal) point in cut edge 
       EDGExyBANK(4,2,1) = 0_fp !x comp of second point in cut edge 
       EDGExyBANK(4,2,2) = 1_fp !y comp of second point in cut edge 
       !
       ! wet polygon
       !
       Nwet=Nwet+1
       INTwx(Nwet) = 0._fp
       INTwy(Nwet) = INTy(Ndry) 
    else
       Ndry=Ndry+1
       INTx(Ndry) = alpha/n1-n2/n1 !(alpha/n2-1.d0)*n2/n1
       if (INTx(Ndry).lt.0._fp) then ! I was having rounding problems, intx(Ndry)= 58.9545661603361	-58.954566160336= -5.684341886080801D-014	NEGATIVE!
          INTx(Ndry) = 0._fp
       elseif (INTx(Ndry).gt.1._fp) then !I guess also this could happen
          INTx(Ndry) = 1._fp
       endif
       INTy(Ndry) = 1_fp   
       EDGlenDRY(3) = 1_fp-INTx(Ndry)   
       EDGlenDRY(4) = 0_fp
       EDGExyBANK(3,1,1) = INTx(Ndry) !x comp of first (not-nodal) point in cut edge 
       EDGExyBANK(3,1,2) = INTy(Ndry) !y comp of first (not-nodal) point in cut edge 
       EDGExyBANK(3,2,1) = 1_fp !x comp of second point in cut edge
       EDGExyBANK(3,2,2) = 1_fp !y comp of second point in cut edge
       EDGExyBANK(4,1,1) = 0_fp !x comp of first  node
       EDGExyBANK(4,1,2) = 1_fp !y comp of first  node
       EDGExyBANK(4,2,1) = 0_fp !x comp of second node
       EDGExyBANK(4,2,2) = 0_fp !y comp of second node
       L2 = 3          
       edgeTYP(4) = 3 !wet edge
       if (comparereal(EDGlenDRY(3),1._fp).ne.0) then 
          edgeTYP(3) = 0 !cut edge
       else ! in this case I have the dry region exacting intersecting the vertex, so it is not a cut edge but a  dry one
          edgeTYP(3) = -2 !dey edge
       endif
       Nwet=Nwet+1
       INTwx(Nwet) = 0._fp
       INTwy(Nwet) = 1._fp
       Nwet=Nwet+1
       INTwx(Nwet) = INTx(Ndry) 
       INTwy(Nwet) = 1._fp
    endif
    !
end subroutine ALPHAvof