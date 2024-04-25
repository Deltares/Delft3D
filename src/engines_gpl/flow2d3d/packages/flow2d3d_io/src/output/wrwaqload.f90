!> \file
!! Writes the last 'walking' record.
!! Reads the intermediate sources file and writes the final sources file
      subroutine wrwaqload ( nsrc   , nmax   , mmax   , kmax     , mnksrc , &
     &                       nolay  , nobrk  , nowalk , iwlk     , isaggr , &
     &                       zmodel , itim   , ksrwaq , lunsrctmp,lunwlk  , &
     &                       lunsrc )
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      use precision
      implicit none
!
!           Global variables
!
      integer(4) nsrc                      !<  Number of loads
      integer(4) nmax                      !<  Dimensions of the grid
      integer(4) mmax                      !<  Dimensions of the grid
      integer(4) kmax                      !<  Dimensions of the grid
      integer(4) mnksrc(7,nsrc)            !<  locations and type
      integer(4) nolay                     !<  number of WAQ layers
      integer(4) nobrk                     !<  number of breakpoints in work file
      integer(4) nowalk                    !<  number of walking discharges
      integer(4) iwlk(nsrc)                !<  their number in the list of dicharges
      integer(4) isaggr(nmax,mmax,kmax)    !<  horizontal grid aggregation pointer
      logical    zmodel                    !<  true if z-model feature is used
      integer(4) itim                      !<  time stamp of the last record
      integer(4) ksrwaq(2*nsrc)            !<  Variable lowest active layer (z-model-only)
      integer(4) lunsrctmp, lunwlk, lunsrc !<  file unit number to an output file
!
!           Local variables
!
      integer(4) iseg, isrc, ilay, nsr2    !!  loop counters
      integer(4) nosrc                     !!  number of sources in the work file (inclusive of dry layers)
      integer(4) m, n, k, i, iwp           !!  help variables
      integer(4), allocatable :: iwpnt (:) !!  pointer from nsrc to nosrc
      real   (4), allocatable :: awaste(:) !!  nosrc values read from the work file
      integer(4) itwrk                     !!  time in DDDHHMMSS and help variable
      integer(4) istat                     !!  allocate return status
!
!! executable statements -------------------------------------------------------
!
!             trivial

      if ( nsrc .eq. 0 ) return

!             last record walking discharges

      if ( nowalk .gt. 0 ) then
         m = mnksrc(1,iwlk(1))
         n = mnksrc(2,iwlk(1))
         k = mnksrc(3,iwlk(1))
         iseg = isaggr(n,m,k)
         write ( lunwlk , '(i12/4i12)' ) itim, iseg, m, n, k
         do i = 2,nowalk
            m = mnksrc(1,iwlk(i))
            n = mnksrc(2,iwlk(i))
            k = mnksrc(3,iwlk(i))
            iseg = isaggr(n,m,k)
            write ( lunwlk , '(4I12)' )    iseg, m, n, k
         enddo
      endif
      close ( lunwlk )

!             count the number of wasteloads in the file and
!             make the pointer from the nsrc towards the nosrc

      allocate ( iwpnt (nsrc*2)  , stat=istat )  ! pointer
      if (istat/=0) then
         write(*,*) '*** ERROR: wrwaqload: memory allocation error'
         return
      endif
      iwpnt = 0
      nosrc = 0
      nsr2  = 0
      do isrc = 1, nsrc
         if (mnksrc(3,isrc) == -1) cycle ! awkward disabling of discharges outside partition when running parallel
         if (mnksrc(7,isrc) == 4 .or. mnksrc(7,isrc) == 5 .or. mnksrc(7,isrc) == 8) cycle ! skip e,d and f culverts, output is not correct
         k = mnksrc(3,isrc)
         iwpnt(isrc) = nosrc + 1
         if ( k .eq. 0 ) then
            nosrc = nosrc + nolay
            if ( zmodel ) then
               nsr2 = nsr2 + ksrwaq(isrc)
            else
               nsr2 = nsr2 + nolay
            endif
         else
            nosrc = nosrc + 1
            nsr2  = nsr2  + 1
         endif
      enddo
      do isrc = 1, nsrc
         if (mnksrc(3,isrc) ==  -1 ) cycle   ! awkward disabling of discharges outside partition when running parallel
         if (mnksrc(7,isrc) == 4 .or. mnksrc(7,isrc) == 5 .or. mnksrc(7,isrc) == 8) cycle ! skip e,d and f culverts, output is not correct
         if (mnksrc(7,isrc) .le. 1 ) cycle   ! no inlet outlet
         k = mnksrc(6,isrc)
         iwpnt(isrc+nsrc) = nosrc + 1
         if ( k .eq. 0 ) then
            nosrc = nosrc + nolay
            if ( zmodel ) then
               nsr2 = nsr2 + ksrwaq(nsrc+isrc)
            else
               nsr2 = nsr2 + nolay
            endif
         else
            nosrc = nosrc + 1
            nsr2  = nsr2  + 1
         endif
      enddo

      allocate ( awaste(nosrc) , stat=istat )  ! array of values
      if (istat/=0) then
         write(*,*) '*** ERROR: wrwaqload: memory allocation error'
         return
      endif
      awaste = 0.0

!             write header of waste file

      rewind( lunsrctmp )                         ! rewind the work file
      write ( lunsrc , '(   A)' ) ' SECONDS    '
      write ( lunsrc , '(   A)' ) '    3    ; time dependent sources'
      write ( lunsrc , '(   A)' ) '    1    ; block function'
      write ( lunsrc , '(I5,A)' )  nsr2,                                 &
     &                            '    ; no. of sources in this block'
      write ( lunsrc , '(I4,7I5)' ) ( k,k=1,nsr2 )
      write ( lunsrc , '(I7,A)' )  nobrk+1,                              &
     &                            '  ; number of breakpoints'
      write ( lunsrc , '(   A)' ) '1.0 1.0  ; scale factors'

!             finalisation. read and reshuffel the data

      do n = 1, nobrk+1
         if ( n .ne. nobrk+1 ) then
            read  ( lunsrctmp , '(i10  )' ) itwrk
         else
            itwrk = itim
         endif
         write ( lunsrc , '(i10,A)' ) itwrk,'          ; breakpoint time'
         if ( n .ne. nobrk+1 ) then
            do isrc = 1, nosrc
               read  ( lunsrctmp , '(24X,E15.6)' ) awaste(isrc)    !  one timestep
            enddo
         endif
         nsr2 = 1
         do isrc = 1, nsrc
            if (mnksrc(3,isrc) == -1) cycle ! awkward disabling of discharges outside partition when running parallel
            if (mnksrc(7,isrc) == 4 .or. mnksrc(7,isrc) == 5 .or. mnksrc(7,isrc) == 8) cycle ! skip e,d and f culverts, output is not correct
            k = mnksrc(3,isrc)
            iwp = iwpnt(isrc)
            write ( lunsrc , '(6X,E15.6,A,I4)' ) awaste(iwp),               &
     &                                  '    1.0  ; SOURCE:',nsr2
            nsr2 = nsr2 + 1
            if ( zmodel .and. k .eq. 0 ) then
               do ilay = 1, ksrwaq(isrc)-1
                  write ( lunsrc , '(6X,E15.6,A,I4)' ) awaste(iwp+ilay),    &
     &                                  '    1.0  ; SOURCE:',nsr2
                  nsr2 = nsr2 + 1
               enddo
            endif
         enddo
         do isrc = 1, nsrc
            if (mnksrc(3,isrc) ==  -1) cycle   ! awkward disabling of discharges outside partition when running parallel
            if (mnksrc(7,isrc) == 4 .or. mnksrc(7,isrc) == 5 .or. mnksrc(7,isrc) == 8) cycle ! skip e,d and f culverts, output is not correct
            if (mnksrc(7,isrc) .le. 1) cycle   ! not an intake - outfall or other construct
            k = mnksrc(6,isrc)
            iwp = iwpnt(isrc+nsrc)
            write ( lunsrc , '(6X,E15.6,A,I4)' ) awaste(iwp),               &
     &                                  '    1.0  ; SOURCE:',nsr2
            nsr2 = nsr2 + 1
            if ( zmodel .and. k .eq. 0 ) then
               do ilay = 1, ksrwaq(nsrc+isrc)-1
                  write ( lunsrc , '(6X,E15.6,A,I4)' ) awaste(iwp+ilay),    &
     &                                  '    1.0  ; SOURCE:',nsr2
                  nsr2 = nsr2 + 1
               enddo
            endif
         enddo
         if ( .not. zmodel ) then               ! zmodel alraedy dealt with
            do ilay = 1, nolay-1
               do isrc = 1, nsrc
                  if (mnksrc(3,isrc) ==  -1) cycle   ! awkward disabling of discharges outside partition when running parallel
                  if (mnksrc(7,isrc) == 4 .or. mnksrc(7,isrc) == 5 .or. mnksrc(7,isrc) == 8) cycle ! skip e,d and f culverts, output is not correct
                  if (mnksrc(3,isrc) .eq. 0) then    ! depth average loads
                     iwp = iwpnt(isrc)
                     write ( lunsrc , '(6X,E15.6,A,I4)' ) awaste(iwp+ilay), &
     &                                  '    1.0  ; SOURCE:',nsr2
                     nsr2 = nsr2 + 1
                  endif
               enddo
               do isrc = 1, nsrc
                  if (mnksrc(3,isrc) ==  -1) cycle   ! awkward disabling of discharges outside partition when running parallel
                  if (mnksrc(7,isrc) == 4 .or. mnksrc(7,isrc) == 5 .or. mnksrc(7,isrc) == 8) cycle ! skip e,d and f culverts, output is not correct
                  if (mnksrc(7,isrc) .le. 1) cycle   ! not an intake - outfall or other construct
                  if (mnksrc(6,isrc) .eq. 0) then    ! depth average loads
                     iwp = iwpnt(isrc+nsrc)
                     write ( lunsrc , '(6X,E15.6,A,I4)' ) awaste(iwp+ilay), &
     &                                  '    1.0  ; SOURCE:',nsr2
                     nsr2 = nsr2 + 1
                  endif
               enddo
            enddo
         endif
      enddo

      close ( lunsrctmp, STATUS='DELETE' )
      close ( lunsrc )
      deallocate ( iwpnt  )
      deallocate ( awaste )
      end subroutine wrwaqload
