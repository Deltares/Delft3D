!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_spcsd
      use m_waq_precision


      implicit none

      contains


!  *********************************************************************
!  *  SUBROUTINE FOR ORDERING EXTINCTION COEFFICIENTS AND DETERMINING  *
!  *         EXISTENCE OF SPECIES IN COEFFICIENT INTERVALS             *
!  *********************************************************************

      subroutine spcsd(xvec,rvec,aco,extlim,extb,ni)

      use bloom_data_dim
      use bloom_data_size 
      use bloom_data_phyt  

      implicit none

      real(kind=dp) ::aco(mt,mt),rvec(2*mt),svec(2*mt),dvec(2*mt),xvec(2*mt)
      real(kind=dp) ::extb, rtemp, extlim
      
      integer(kind=int_wp) ::i, ij, il, j, jk, k, k3, m, n, ni, nn

!  Initialize.
      do i=1,nuspec
         do j=1,nuspec
            aco(i,j)=1.0
         end do
      end do
      do i=1,2*nuspec
         dvec(i)=0.0
         svec(i)=0.0
         rvec(i)=0.0
      end do

!  Determine type roots, subtract EXTB * SDMIX(I).
!  Update nov 4 1992:
!  Use absolute value of SDMIX; SDMIX can be negative for types attached
!  to the bottom.
      n=0
      do i=1,nuspec
         ij=2*i-1
         do k3=1,2
            n=n+1
            jk=ij+k3-1
            svec(n)=xvec(jk)-extb*abs(sdmix(i))
            dvec(n)=svec(n)
         end do
      end do

!  Order values in vector DVEC.
      nn=n-1
      do i=1,nn
         il=i+1
         do j=il,n
            if (dvec(i) <= dvec(j)) cycle
            rtemp=dvec(i)
            dvec(i)=dvec(j)
            dvec(j)=rtemp
         end do
      end do

!
!  Are there any valid intervals?
      if (dvec(n) <= 0.0) go to 130

!  Eliminate intervals whose maximum root is either negative or
!  (in a dynamic run) smaller than the extinction of the remaining
!  biomass.
      do k=1,n
         if (dvec(k) > extlim) exit
      end do
      if (k>1) k=k-1

!  Eliminate duplicates in output vector.
      m=1
      rvec(1)=dvec(k)
      do i=k,nn
         if (dvec(i+1) <= dvec(i)) cycle
         m=m+1
         rvec(m)=dvec(i+1)
      end do

!  Determine types in intervals.
      ni=m-1
      do i=1,nuspec
         ij=2*i-1
         if ((svec(ij+1) < 0.)) cycle
         do j=1,ni
            if ((svec(ij) <= rvec(j)) .and. (svec(ij+1) >= rvec(j+1))) aco(j,i)=0.0
         end do
      end do
      return
  130 ni=0
      return
      end

      end module m_spcsd
