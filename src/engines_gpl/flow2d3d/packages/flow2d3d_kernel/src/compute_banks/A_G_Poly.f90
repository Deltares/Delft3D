subroutine A_G_Poly(X,Y,NVERT,area,xG,yG,typeOUT,lunscr, gdp) 
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
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   compute the area and/or centroid of a simple (i.e. not intersecting) polygon 
!               (can be concave or convex).
!               Note: the first vertex has to be coincident with the last 
!               NOte: the orientation can be either clockwise or anticlockwise
!               typeOUT = 1: compute only area
!               typeOUT = 2: compute both area and centroid (to compute centroid you have to compute area)
!
!  Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    use precision
!
    implicit none
!
    type(globdat),target :: gdp
!
! global variables
!
    integer                    :: lunscr
    integer, intent(in)        :: nVERT
    integer, intent(in)        :: typeOUT
    REAL(fp), intent(in)       :: X(nVERT) 
    REAL(fp), intent(in)       :: Y(nVERT)
    REAL(fp), intent(out)      :: area
    REAL(fp), intent(out)      :: xG
    REAL(fp), intent(out)      :: yG
    REAL(fp)                   :: prov
!
! local variables
!
    integer                    :: I,I1,I2
    real(fp),parameter         :: oneSIXTH = 1._fp/6._fp
    logical                    :: found 
!
!
! executable statements -------------------------------------------------------
!
    select case(TYPEout) 
    case(1)
       !
       ! only area
       !
       area = 0._fp
       do I=1,nVERT-1     
          area = area + X(I)*Y(I+1)-X(I+1)*Y(I)
       enddo
       !if ((comparereal(xG,27._fp).eq.0).and.(comparereal(yG,30._fp).eq.0)) write(87878787,'(i8,20f35.20)') nst,area,(X(I),Y(I),I=1,nVERT)
       area = abs(area)*0.5_fp
    case(11) 
       !
       ! only area with sign
       !
       area = 0._fp
       do I=1,nVERT-1     
          area = area + X(I)*Y(I+1)-X(I+1)*Y(I)
       enddo
       !if ((comparereal(xG,27._fp).eq.0).and.(comparereal(yG,30._fp).eq.0)) write(87878787,'(i8,20f35.20)') nst,area,(X(I),Y(I),I=1,nVERT)
       area = area*0.5_fp
    case(2)
       !
       ! both area and centroid
       !
       xG = 0._fp
       yG = 0._fp
       area = 0._fp
       do I=1,nVERT-1  
          prov =   X(I)*Y(I+1)-X(I+1)*Y(I)   
          area = area + prov
          xG = xG + prov * (X(I)   + X(I+1))
          yG = yG + prov * (Y(I+1) + Y(I)  ) 
       enddo
       !
       ! note  if vertices are ordered clockwise, the value given by the area formula will be negative but correct in absolute value, 
       ! but when calculating xG and yG the signed value of A (which in this case is negative) should be used. 
       ! This is commonly called the Surveyor's Formula.
       !
       area =  area*0.5_fp 
       if (comparereal(area,0._fp).ne.0) then
          xG = xG*oneSIXTH/area
          yG = yG*oneSIXTH/area
       else 
          !
          ! handle only the degenerate case for a quadrilateral in which 2 ajacent couple of points or all 4 points concides 
          ! (useful for degenerated grid with coincident points)
          !
          if (nVERT-1.eq.4) then 
             !
             ! quadrilateral
             !
             found = .false.
             do I=1,4
                if (comparereal(X(I),X(I+1)).EQ.0.and.comparereal(Y(I),Y(I+1)).EQ.0) then    
                   found =.true.
                   I1=I                   
                   exit              
                endif
             enddo
             if (.not.found) then
                 write(*,*) 'Erros: Area is zero but neigbour points are not coincident'
                call d3stop(1,gdp)
             endif
             !
             ! since exit does not increase I, it only exits the cycle!!!
             !
             if (comparereal(X(I1+2),X(I1+3)).EQ.0.and.comparereal(Y(I1+2),Y(I1+3)).EQ.0) then 
                I2=I1+2
                !
                ! this works also if they are all 4 coincident
                !
                xG = 0.5_fp*(X(I1)+X(I2))
                yG = 0.5_fp*(Y(I1)+Y(I2))
             else
                !
                ! there are not 2 coincident couples, exit.
                !
                write(*,*) 'Impossible to compute baricenter since Area is zero'
                call d3stop(1,gdp)
             endif   
          else
             write(*,*) 'Impossible to compute baricenter since Area is zero'
             call d3stop(1,gdp)
          endif
       endif
       area = abs(area)
    case default
      write(lunscr,*) 'Wrong value of typeOUT.' 
      call d3stop(1,gdp)
    end select
    !
end subroutine A_G_Poly