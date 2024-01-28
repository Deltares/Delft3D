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

! make a step to the next sample in a sample path
subroutine makestep_samplepath(ipprev, ipcur, ipnext, Nsub, ipsub, ierror)
   use m_samples
   use m_samples_refine
   use m_missing
   use geometry_module, only: dbdistance, dcosphi
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer,                            intent(in)    :: ipprev         !< previous sample point
   integer,                            intent(in)    :: ipcur          !< current  sample point
   integer,                            intent(out)   :: ipnext         !< next     sample point
   integer,                            intent(inout) :: Nsub           !< array size of ipsub (in), number of samples in subpath to next sample point (out)
   integer, dimension(Nsub),           intent(out)   :: ipsub          !< samples in subpath to next sample point
   integer,                            intent(out)   :: ierror         !< no errors (0), need to realloc ipsub (-newsize) or other error (1)

   integer                                           :: i, j, i0, i1, j0, j1, ip, iploc, icur, jcur, num, Nsub0
   integer                                           :: isub, jsub, i00, i11, j00, j11, ii, jj, ip1, ip2

   double precision                                  :: dcsphi, disub, djsub, zs_ave, zs_max
   double precision                                  :: Dh, Dzs

   integer, parameter                                :: Nwidth=5       !  number of sample widths considered

   integer                                           :: Nlist, jatoosteep
   integer, dimension(2*(Nwidth+1))                  :: iplist

   ipnext = 0

   ierror = 1

   Nsub0 = Nsub

   jcur = ipcur/MXSAM+1
   icur = ipcur - (jcur-1)*MXSAM

   i0 = max(icur-Nwidth,1)
   i1 = min(icur+Nwidth,MXSAM)
   j0 = max(jcur-Nwidth,1)
   j1 = min(jcur+Nwidth,MYSAM)

   zs_max = -1d99

!  determine sample meshwidth
!  i-dir
   ip1 = i0 + (jcur-1)*MXSAM
   ip2 = i1 + (jcur-1)*MXSAM
   Dh = dbdistance(xs(ip1),ys(ip1),xs(ip2),ys(ip2),jsferic, jasfer3D, dmiss)/max(dble(i1-i0),1d0)
!  j-dir
   ip1 = icur + (j1-1)*MXSAM
   ip2 = icur + (j1-1)*MXSAM
   Dh = max(dh,dbdistance(xs(ip1),ys(ip1),xs(ip2),ys(ip2),jsferic, jasfer3D, dmiss)/max(dble(j1-j0),1d0))

   do i=i0,i1
      do j=j0,j1
!         if ( i.ne.i0 .and. i.ne.i1 .and. j.ne.j0 .and. j.ne.j1 ) cycle

         if ( i-i0.gt.1 .and. i1-i.gt.1 .and. j-j0.gt.1 .and. j1-j.gt.1 ) cycle

         ip = i + (j-1)*MXSAM

         if ( ip.eq.ipcur ) cycle

!        next sample may never have DMISS coordinates/value
         if ( xs(ip).eq.DMISS .or. zs(ip).eq.DMISS ) cycle

!        check angle with previous step
         if ( ipprev.ne.ipcur .and. ipprev.gt.0 ) then
            dcsphi = dcosphi(xs(ipprev),ys(ipprev),xs(ipcur),ys(ipcur),xs(ipcur),ys(ipcur),xs(ip),ys(ip), jsferic, jasfer3D, dxymis)

            if ( dcsphi.lt.0.5d0 ) cycle
         end if

!        make subbath
         Nlist = 0
         i00 = min(icur,i)
         i11 = max(icur,i)
         j00 = min(jcur,j)
         j11 = max(jcur,j)

         do isub=i00,i11
            if ( i.ne.icur ) then
               djsub = dble(isub-icur)/dble(i-icur)*dble(j-jcur) + jcur
            else
               djsub = 0d0
            end if
            do jsub=j00,j11
               if ( j.ne.jcur ) then
                  disub = dble(jsub-jcur)/dble(j-jcur)*dble(i-icur) + icur
               else
                  disub = 0d0
               end if
               if ( abs(isub-disub).lt.1d0 .or. abs(jsub-djsub).lt.1d0 ) then
                  Nlist = Nlist+1
                  iplist(Nlist) = isub + (jsub-1)*MXSAM
               end if
            end do
         end do

!        compute average sample value
         zs_ave = 0d0
         num = 0
         do ii=1,Nlist
            iploc = iplist(ii)
            jsub  = iploc/MXSAM+1
            isub  = iploc-(jsub-1)*MXSAM
            if ( zs(iploc).ne.DMISS ) then
               num = num+1
               zs_ave=zs_ave+zs(iploc)
            end if
         end do
         zs_ave = zs_ave/dble(max(num,1))

!!       plot samples in subpath
!        do isub=1,Nlist
!           ipsub = iplist(isub)
!           call cirr(xs(ipsub),ys(ipsub),31)
!        end do
!        call qnerror(' ', ' ', ' ')
!        do isub=1,Nlist
!           ipsub = iplist(isub)
!           call cirr(xs(ipsub),ys(ipsub),0)
!        end do

!       check for maximum average sample value
        if ( zs_ave.gt.zs_max .and. num.gt.1 ) then

!           27-06-12: deactivated gradient check
!!          gradient may not be too large
!           jatoosteep = 0
!           Dzs = maxval(zs(iplist(1:Nlist)), MASK=zs(iplist(1:Nlist)).ne.DMISS)
!           Dzs = Dzs-minval(zs(iplist(1:Nlist)), MASK=zs(iplist(1:Nlist)).ne.DMISS)
!           if ( abs(Dzs).gt.0.25d0*Dh ) then
!              jatoosteep = 1
!              cycle
!           else
!              jatoosteep = 0
!           end if

!           if ( icur.eq.1294 .and. jcur.eq.1051 ) then
!              write(6,"(2I, $)") i-icur, j-jcur
!              do ii=1,Nlist
!                 write(6,"(F15.5, $)") zs(iplist(ii))
!              end do
!              write(6,*)
!           end if

           ipnext = ip
           zs_max = zs_ave
!          reallocate if necessary
           if ( Nlist.gt.ubound(ipsub,1) ) then
              Nsub = int(1.2d0*dble(Nlist))+1
              ierror = -Nsub
              goto 1234
           end if
           Nsub = 0
           do isub=1,Nlist
              Nsub = Nsub+1
              ipsub(isub) = iplist(isub)
           end do
        end if

      end do
   end do

!  plot next sample
   if ( ipnext.gt.0 ) then
      call cirr(xs(ipnext),ys(ipnext),31)
      call setcol(31)
      call movabs(xs(ipcur),ys(ipcur))
      call lnabs(xs(ipnext),ys(ipnext))
   end if

   ierror = 0
1234 continue

   return
end subroutine makestep_samplepath
