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

 subroutine addsorsin(filename, area, ierr)

 use fm_external_forcings_data
 use m_polygon
 use m_flow
 use m_GlobalParameters, only: INDTP_ALL

 use m_missing
 use unstruc_messages
 use dfm_error
 use geometry_module, only: normalin
 use m_sferic, only: jsferic, jasfer3D
 use MessageHandling, only: IdLen
 use m_find_flownode, only: find_nearest_flownodes

 implicit none

 character (len=*), intent(in)  :: filename
 double precision,  intent(in)  :: area
 integer,           intent(out) :: ierr
 integer   :: minp, k, kk, kb, kt, kk2, n1, n2, i, jakdtree, kdum(1)
 character (len=IdLen) :: tmpname(1)

 ierr = DFM_NOERR

 call oldfil(minp, filename)
 call reapol(minp, 0)

 if (npl == 0) return

 numsrc = numsrc + 1
 call reallocsrc(numsrc)

 ! set the coordinates of source/sink
 xsrc(numsrc,1:npl) = xpl(1:npl)
 ysrc(numsrc,1:npl) = ypl(1:npl)
 nxsrc(numsrc)      = npl
 kk = 0; kk2 = 0

 ! Strip off trailing file extension .pli
 n2  = index(filename,'.', .true.) - 1
 if (n2 < 0) then
    n2 = len_trim(filename)
 end if

 ! Strip off leading path /dir/name/bnd/
 n1  = index(filename(1:n2),'\', .true.) ! Win
 if (n1 == 0) then
     n1 = index(filename(1:n2),'/', .true.) ! Or try UX
 end if

 ! Store sink/source name for waq
 srcname(numsrc) = filename(n1+1:n2)

 ! call inflowcell(xpl(npl), ypl(npl), kk2) ! TO: Source
 tmpname(1) = filename(n1+1:n2) // ' source'
 jakdtree = 0
 kdum(1)  = 0
 if (xpl(npl) .ne. -999.999d0) then
    call find_nearest_flownodes(1,xpl(npl),ypl(npl),tmpname(1),kdum(1),jakdtree,-1, INDTP_ALL) ; kk2 = kdum(1)
 endif

 ! Support point source/sinks in a single cell if polyline has just one point (npl==1)
 if (npl == 1) then

    kk = 0 ! Only keep the source-side (kk2), and disable momentum discharge
    if (area /= dmiss .and. area /= 0d0) then
       ! User specified an area for momentum discharge, but that does not apply to POINT sources.
       write (msgbuf, '(a,a,a,f8.2,a)') 'Source-sink for ''', trim(filename), ''' is a POINT-source. Nonzero area was specified: ', area, ', but area will be ignored (no momentum discharge).'
       call warn_flush()
    end if
    arsrc (numsrc) = 0d0
 else ! Default: linked source-sink, with polyline npl >= 2
    ! call inflowcell(xpl(1) , ypl(1)  , kk) ! FROM: sink
    tmpname = filename(n1+1:n2) // ' sink'
    kdum(1) = 0
    if (xpl(1) .ne. -999.999d0) then
       call find_nearest_flownodes(1,xpl(1),ypl(1),tmpname(1),kdum(1),jakdtree,-1,INDTP_ALL) ; kk = kdum(1)
    endif

    if (kk.ne.0 .or. kk2.ne.0) then
       arsrc (numsrc) = area
    endif
 end if

 if (kk == 0 .and. kk2 == 0) then
    write (msgbuf, '(a,a)') 'Source+sink is outside model area for ', trim(filename)
    call warn_flush()
    ierr = DFM_NOERR
    goto 8888
 endif

 ksrc (1,numsrc) = kk
 zsrc (1,numsrc) = zpl(1)
 zsrc2(1,numsrc) = zpl(1)

 ksrc (4,numsrc) = kk2
 zsrc (2,numsrc) = zpl(npl)
 zsrc2(2,numsrc) = zpl(npl)

 if (kk > 0) then
    if ( allocated(dzL) ) then
       if (dzL(1) .ne. dmiss) then
           zsrc2(1,numsrc) = dzL(1)
       endif
    endif
    ! Determine angle (sin/cos) of 'from' link (=first segment of polyline)
    if (npl > 1) then
      call normalin (xpl(1), ypl(1), xpl(2), ypl(2), cssrc(1,numsrc), snsrc(1,numsrc), xpl(1), ypl(1), jsferic, jasfer3D, dxymis)
    end if

    do i = 1,numsrc-1
       if (      ksrc(1,i) .ne. 0 .and. kk == ksrc(1,i) ) then
          write (msgbuf, '(4a)')  'FROM point of ', trim (srcname(numsrc)),' coincides with FROM point of ', trim(srcname(i)) ;  call warn_flush()
       else if ( ksrc(4,i) .ne. 0 .and. kk == ksrc(4,i) ) then
          write (msgbuf, '(4a)')  'FROM point of ', trim (srcname(numsrc)),' coincides with TO   point of ', trim(srcname(i)) ;  call warn_flush()
       endif
    enddo

 endif

 if (kk2 > 0) then
    if ( allocated(dzL) ) then
       if (dzL(npl) .ne. dmiss) then
           zsrc2(2,numsrc) = dzL(npl)
       endif
    endif
    ! Determine angle (sin/cos) of 'to' link (=first segment of polyline)
    if (npl > 1) then
       call normalin (xpl(npl-1), ypl(npl-1), xpl(npl), ypl(npl), cssrc(2,numsrc), snsrc(2,numsrc), xpl(NPL), ypl(NPL), jsferic, jasfer3D, dxymis)
    endif

    do i = 1,numsrc-1
       if (      ksrc(1,i) .ne. 0 .and. kk2 == ksrc(1,i) ) then
          write (msgbuf, '(4a)')  'TO point of ', trim (srcname(numsrc)),' coincides with FROM point of ', trim(srcname(i)) ;  call warn_flush()
       else if ( ksrc(4,i) .ne. 0 .and. kk2 == ksrc(4,i) ) then
          write (msgbuf, '(4a)')  'TO point of ', trim (srcname(numsrc)),' coincides with TO   point of ', trim(srcname(i)) ;  call warn_flush()
       endif
    enddo

 endif

8888 continue

 end subroutine addsorsin
