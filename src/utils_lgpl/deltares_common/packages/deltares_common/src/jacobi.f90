!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> From: NUMERICAL RECIPES IN FORTRAN 77, sect. 11.1
!>
!>    Computes all eigenvalues and eigenvectors of a real symmetric matrix a, which is of size n
!>    by n, stored in a physical np by np array. On output, elements of a above the diagonal are
!>    destroyed. d returns the eigenvalues of a in its first n elements. v is a matrix with the same
!>    logical and physical dimensions as a, whose columns contain, on output, the normalized
!>    eigenvectors of a. nrot returns the number of Jacobi rotations that were required.
SUBROUTINE jacobi(a,n,np,d,v,nrot)
   implicit none
   INTEGER, intent(in)                :: n, np
   integer, intent(out)               :: nrot
   double precision, intent(inout)    :: a(np,np),d(np),v(np,np)
   integer                            :: num_rows
   PARAMETER (num_rows=500)
   INTEGER i,ip,iq,j
   double precision c,g,h,s,sm,t,tau,theta,tresh,b(num_rows),z(num_rows)
   r12:do ip=1,n               ! Initialize to the identity matrix.
      r11:do iq=1,n
         v(ip,iq)=0.
      enddo r11
      v(ip,ip)=1.
   enddo r12
   r13:do ip=1,n
      b(ip)=a(ip,ip)          ! Initialize b and d to the diagonal of a.
      d(ip)=b(ip)
      z(ip)=0.                ! This vector will accumulate terms of the form tapq
   enddo r13                  !     as in equation (11.1.14).
   nrot=0
   r24:do i=1,50
      sm=0.
      r15:do ip=1,n-1          ! Sum off-diagonal elements.
         r14:do iq=ip+1,n
            sm=sm+abs(a(ip,iq))
         enddo r14
      enddo r15
      if(sm == 0.)return      ! The normal return, which relies on quadratic conver
      if(i < 4)then          !    gence to machine underflow.
         tresh=0.2*sm/n**2    !  ...on the first three sweeps.
      else
         tresh=0.             !    ...thereafter.
      endif
      r22:do ip=1,n-1
         r21:do iq=ip+1,n
            g=100.*abs(a(ip,iq))
!              After four sweeps, skip the rotation if the o-diagonal element is small.
            if((i > 4).and.(abs(d(ip))+g == abs(d(ip))) .and.(abs(d(iq))+g == abs(d(iq))))then
                  a(ip,iq)=0.
            else if(abs(a(ip,iq)) > tresh)then
               h=d(iq)-d(ip)
               if(abs(h)+g == abs(h))then
                  t=a(ip,iq)/h      ! t = 1=(2)
               else
                  theta=0.5*h/a(ip,iq)    ! Equation (11.1.10).
                  t=1./(abs(theta)+sqrt(1.+theta**2))
                  if(theta < 0.)t=-t
               endif
               c=1./sqrt(1+t**2)
               s=t*c
               tau=s/(1.+c)
               h=t*a(ip,iq)
               z(ip)=z(ip)-h
               z(iq)=z(iq)+h
               d(ip)=d(ip)-h
               d(iq)=d(iq)+h
               a(ip,iq)=0.
               r16:do j=1,ip-1             ! Case of rotations 1  j < p.
                  g=a(j,ip)
                  h=a(j,iq)
                  a(j,ip)=g-s*(h+g*tau)
                  a(j,iq)=h+s*(g-h*tau)
               enddo r16
               r17:do j=ip+1,iq-1          ! Case of rotations p < j < q.
                  g=a(ip,j)
                  h=a(j,iq)
                  a(ip,j)=g-s*(h+g*tau)
                  a(j,iq)=h+s*(g-h*tau)
               enddo r17
               r18:do j=iq+1,n             ! Case of rotations q < j  n.
                  g=a(ip,j)
                  h=a(iq,j)
                  a(ip,j)=g-s*(h+g*tau)
                  a(iq,j)=h+s*(g-h*tau)
               enddo r18
               r19:do j=1,n
                  g=v(j,ip)
                  h=v(j,iq)
                  v(j,ip)=g-s*(h+g*tau)
                  v(j,iq)=h+s*(g-h*tau)
               enddo r19
               nrot=nrot+1
            endif
         enddo r21
      enddo r22
      r23:do ip=1,n
         b(ip)=b(ip)+z(ip)
         d(ip)=b(ip)          ! Update d with the sum of tapq,
         z(ip)=0.             ! and reinitialize z.
      enddo r23
   enddo r24
   write(*,*) 'too many iterations in jacobi'
   read(*,*)
   return
END
