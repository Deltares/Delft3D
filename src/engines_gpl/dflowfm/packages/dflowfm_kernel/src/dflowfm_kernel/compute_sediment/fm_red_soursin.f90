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

   subroutine fm_red_soursin()
   !
   !!--description-----------------------------------------------------------------
   !
   !    Function: Reduces sourse and sink terms to avoid large
   !              bed level changes
   !
   !!--declarations----------------------------------------------------------------
   use precision
   use sediment_basics_module
   use unstruc_files, only: mdia
   use m_flowtimes, only: dts, tstart_user, time1, dnt, tfac
   use m_transport, only: ised1
   use m_flow, only: s0, s1, kmx, zws, zws0
   use m_transport, only: constituents
   use message_module, only: write_warning
   use m_flowgeom
   use m_fm_erosed
   !
   implicit none
   !
   real(fp), dimension(kmx)             :: thick
   !
   ! Local parameters
   !
   integer, parameter  :: reducmessmax = 50
   !
   ! Global variables
   !
   !
   ! Local variables
   !
   integer          :: lstart, reducmesscount, l, ll, nm, kmaxsd, kb, kt
   real(fp)         :: h0, h1, thick0, thick1, dz, reducfac
   character(150)   :: message
   !
   !! executable statements -------------------------------------------------------
   !!
   lstart         = ised1-1
   reducmesscount = 0
   !
   do l = 1, lsed
      ll = lstart + l
      !
      ! Reduction is not applied to mud and not to bedload
      !
      if (tratyp(l) == TRA_COMBINE) then
         if (kmx > 0) then
            do nm = 1, ndx
               call getkbotktop(nm, kb, kt)
               !
               ! Apply reduction factor to source and sink terms if
               ! bottom change is close to threshold fraction water depth and
               ! erosive conditions are expected
               !
               kmaxsd = kmxsed (nm,l)
               h0     = max(0.01_fp, s0(nm) - bl(nm))
               h1     = max(0.01_fp, s1(nm) - bl(nm))
               thick0 = zws0(kmaxsd)-zws0(kmaxsd-1) ! should be safe for z and sigma, was: thick(kmaxsd)*h0
               thick1 = zws(kmaxsd)-zws(kmaxsd-1)   ! thick(kmaxsd)*h1
               dz     = (dts*morfac/cdryb(l)) &
                  & * (sourse(nm, l)*thick0 - (sinkse(nm, l)+sour_im(nm, l))*thick1*constituents(ll, kmaxsd))
               if (abs(dz) > h1*dzmax) then
                  reducfac = (h1*dzmax)/abs(dz)
                  if (reducfac < 0.01 .and. (time1 > tstart_user + tmor * tfac) .and. bedupd) then
                     !
                     ! Only write reduction warning when bed updating is true (and started)
                     ! (otherwise no problem)
                     ! Limit the number of messages with reducmessmax
                     !
                     reducmesscount = reducmesscount + 1
                     if (reducmesscount <= reducmessmax) then
                        write(message,'(a,i0,a,f12.2,a,i0,a,2(f12.0,a),a,i0,a)') &
                           & 'Source and sink term sediment ',l,' reduced with factor', &
                           & 1/reducfac,' node number=(',nm,') at x=', xz(nm),', y=', yz(nm),', after ', int(dnt) , ' timesteps.'
                        call write_warning(message, unit=mdia)
                     endif
                  endif
                  sourse(nm, l)  = sourse(nm, l) *reducfac
                  sour_im(nm, l) = sour_im(nm, l)*reducfac
                  sinkse(nm, l)  = sinkse(nm, l) *reducfac
               endif
               !
               ! Apply reduction factor to source and sink
               ! terms if bottom is closer than user-specified
               ! threshold and erosive conditions are expected
               !
               !
               ! If erosion conditions are expected then apply
               ! reduction factor to sour and sink terms.
               ! estimate sink term based on previous cell
               ! concentration
               !
               if ((sinkse(nm,l)+sour_im(nm,l))*constituents(ll, kmaxsd) < sourse(nm,l)) then
                  sinkse(nm, l)  = sinkse(nm, l) * fixfac(nm,l)
                  sourse(nm, l)  = sourse(nm, l) * fixfac(nm,l)
                  sour_im(nm, l) = sour_im(nm, l)* fixfac(nm,l)
                  rsedeq(nm, l)  = rsedeq(nm, l) * fixfac(nm,l)
               endif
            enddo               ! nm
         else
            do nm = 1, ndx
               !
               h0     = max(0.01_fp, s0(nm) - bl(nm))
               h1     = max(0.01_fp, s1(nm) - bl(nm))
               dz     = (dts*morfac/cdryb(l)) &
                  & * (sourse(nm, l)*h0 - (sinkse(nm, l)+sour_im(nm, l))*h1*constituents(ll, nm))
               !
               if (abs(dz) > h1*dzmax) then
                  reducfac = (h1*dzmax)/abs(dz)
                  if (reducfac < 0.01 .and. (time1 > tstart_user + tmor * tfac) .and. bedupd) then
                     reducmesscount = reducmesscount + 1
                     if (reducmesscount <= reducmessmax) then
                        write(message,'(a,i0,a,f12.2,a,i0,a,2(f12.0,a),i0,a)') &
                           & 'Source and sink term sediment ',l,' reduced with factor', &
                           & 1/reducfac,' node number=(',nm,') at x=', xz(nm),', y=', yz(nm),', after ', int(dnt) , ' timesteps.'
                        call write_warning(message, unit=mdia)
                     endif
                  endif
                  sourse(nm, l)  = sourse(nm, l) *reducfac
                  sour_im(nm, l) = sour_im(nm, l)*reducfac
                  sinkse(nm, l)  = sinkse(nm, l) *reducfac
               endif
               !
               if ((sinkse(nm,l)+sour_im(nm,l))*constituents(ll, nm) < sourse(nm,l)) then
                  sinkse(nm, l)  = sinkse(nm, l) * fixfac(nm,l)
                  sourse(nm, l)  = sourse(nm, l) * fixfac(nm,l)
                  sour_im(nm, l) = sour_im(nm, l)* fixfac(nm,l)
                  rsedeq(nm, l)  = rsedeq(nm, l) * fixfac(nm,l)
               endif
            enddo            ! nm
         endif               ! kmx
      endif                  ! tratyp
   enddo                     ! lsedtot
   if (reducmesscount > reducmessmax) then
      write (mdia,'(12x,a,i0,a)') 'Reduction messages skipped (more than ',reducmessmax,')'
      write (mdia,'(12x,a,f5.0,a,i0)') 'Total number of reduction messages for timestep ', &
         & dnt,' : ', reducmesscount
   endif
   !
   end subroutine fm_red_soursin
