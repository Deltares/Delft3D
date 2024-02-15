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
module m_wrttrk

implicit none

contains


      subroutine wrttrk ( lundia , fout   , filnam , ittrkc , ntrk   ,     &
                          npmax  , xa     , ya     , za     , xyztrk ,     &
                          nosubs , wpart  , track                    )
!
!-----------------------------------------------------------------------
!          Deltares
!
!             module: subroutine wrttrk
!           function: writes the time varying groups (2 & 3) to the
!                     nefis particle tracking file
!
!                     derived from wrtdro drogue subroutine in flow
!                     adapted for particle tracking
!             date  : 30-09-2001
!
!
!        method used:
!               date: 22-11-2000
!         programmer: h.h. leepel/w. ter horst, a.j. mourits
!         cvs header
!            $author: mourits $
!              $date: 22-11-00 9:51 $
!            $source: /u/trisula/cvsroot/trisula/output/wrttrk.f,v $
!          $revision: 14 $
!-----------------------------------------------------------------------
!   calling routines:              postpr
!-----------------------------------------------------------------------
!   called  routines:              filldm
!                                  putgtr
!                                  putgti
!-----------------------------------------------------------------------
!  formal parameters:
!  ------------------
!
!   var. i/o  type dimensions
!   -------------------------
!
! fout     o  l*4                  flag=true if an error is encountered
! ittrkc  i   i*4                  current time counter for the his-
!                                  tory data file
! ittrkf  i   i*4                  integer representation of the start
!                                  time to write the track
!                                  to file (multiples of dt)
! ittrki  i   i*4                  integer representation of the time
!                                  interval to write the track
!                                  to file (multiples of dt)
! lundia  --  i*4                  unit number for diagnostic file
! ntrk    i   i*4                  actual number of released particle tracks
! nopmax  i   i*4                  total number of particle to be released
! trifil  i   ch*(*)               file name for particle tracking file
! xyztrk  i  r*4 3,ntrk            x(1)y(2)z(3)-coordinate corresponding to
!                                  track starting point if
!                                  track is calculated else 999.999
! nosubs  i   i*4                  Number of substances
! wpart   i  r*4 nosubs,ntrk       Mass of all substances for the particles
! track   i  r*4 8,ntrk
!-----------------------------------------------------------------------
!    local variables:
!    ----------------
!
!   var.      type dimensions
!   -------------------------
!
! celidt      i*4                  array with inidices identifying the
!                                  cell from where the reading and the
!                                  writing starts
! elmdes     ch*64 nelmx           array with element description
! elt_dims      i*4  6,nelmx         array containing the dimensions of
!                                  the multiple elements.
! elt_names     ch*16 nelmx           element name defined for the
!                                  nefis-files
! elmqty     ch*16 nelmx           array with element quantity
! elt_types     ch*16 nelmx           array containing the types of the
!                                  elements (real, ch. , etc. etc.)
! elmunt     ch*10 nelmx           array with element physical unit
! errmsg     ch*6                  character var. containing the error
!                                  message to be written to file. the
!                                  message depend on the error.
! filnam     ch*256                help var. for trisula file name
! first       l*4                  first time to define elt_dims
! grnam2/3   ch*16                 data-group name defined for the
!                                  nefis-files
! idummy      i*4  1               help array to read/write nefis files
! ierror      i*4                  local error flag for nefis files
! ival        i*4  2               local array for writing itdate and
!                                  time (:= 00:00:00)
! elt_bytes      i*4  nelmx           array containing the number of by-
!                                  tes of each single elt_types
! wrswch      l*4                  flag to write file
!                                    .true. : write to  file
!                                    .false.: read from file
!-----------------------------------------------------------------------
!
!
      use m_waq_precision       ! single and double precision
      use timers
      use putget_mod      ! explicit interface
      use filldm_mod      ! explicit interface

      implicit none       ! force explicit typing

!     Arguments

!     kind           function         name                      description

      integer  ( int_wp ), intent(in   ) :: lundia                  !< unit nr of the diagnostics file
      logical       , intent(  out) :: fout                    !< output is written
      character( * )                :: filnam                  !< name of the output file
      integer  ( int_wp )                :: ittrkc
      integer  ( int_wp ), intent(in   ) :: ntrk                    !< number of particles to track
      integer  ( int_wp ), intent(in   ) :: npmax                   !< total number of particles
      real     ( real_wp), intent(in   ) :: xa    (npmax)           !< x of the particles
      real     ( real_wp), intent(in   ) :: ya    (npmax)           !< y of the particles
      real     ( real_wp), intent(in   ) :: za    (npmax)           !< z of the particles
      real     ( real_wp), intent(  out) :: xyztrk(  3  , npmax)    !< work array to padd the particles in
      integer       , intent(in   ) :: nosubs                  !< number of particles
      real     ( real_wp), intent(in   ) :: wpart(nosubs, npmax)    !< mass for all substances per particle
      real     ( real_wp), intent(in   ) :: track(  8   , npmax)    !< information on the initial release of the particles

!  declaration and specifications
!
      integer, parameter           :: nelmx  = 4
      logical                      :: wrswch
      character (len=6) :: errmsg
      save          grnam2 ,grnam3 , grnam4, elt_names ,elt_types ,elt_bytes ,celidt
!
!-----nefis statics
!
      logical                                :: first =  .true.
      real   (sp)                            :: default = -999.999
      integer(int_wp ), dimension(6,nelmx ), save :: elt_dims
      save          first
!
!-----end nefis statics
!
      character (len=16)                   ::  grnam2 = 'trk-info-series'
      character (len=16)                   ::  grnam3 = 'trk-series'
      character (len=16)                   ::  grnam4 = 'trk-initial'
      character (len=16), dimension(nelmx) ::  elt_names = (/'ITTRKC','XYZTRK','WPART','TRACK'/)
      character (len=16), dimension(nelmx) ::  elt_types = (/'INTEGER','REAL   ','REAL   ','REAL   '/)
      integer   (int_wp )    , dimension(nelmx) ::  elt_bytes = (/ 4 , 4 , 4 , 4 /)
      integer   (int_wp )                       ::  celidt = 0

      integer   (int_wp ) :: ierr2 , it , nelmx2 , nelmx3, nelmx4
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "wrttrk", ithndl )

!-----------------------------------------------------------------------
!-----initialisation
!-----------------------------------------------------------------------
      nelmx2 = 1
      nelmx3 = 2
      nelmx4 = 1
      ierr2  = 0
      celidt = celidt + 1
!
      errmsg = ' '
      wrswch = .true.
!-----------------------------------------------------------------------
!-----set up the element dimension
!-----------------------------------------------------------------------
      if (first ) then
         first = .false.
         call filldm (elt_dims,1   ,1  ,1     ,0       ,0     ,0     ,0  )
         call filldm (elt_dims,2   ,2  ,3     ,npmax   ,0     ,0     ,0  )
         call filldm (elt_dims,3   ,2  ,nosubs,npmax   ,0     ,0     ,0  )
         call filldm (elt_dims,4   ,2  ,8     ,npmax   ,0     ,0     ,0  )
      endif
!-----------------------------------------------------------------------
!-----group 2, element 1 'ittrkc'
!-----------------------------------------------------------------------
      call putget (filnam    ,grnam2     ,nelmx2    ,elt_names    ,  &
                   elt_dims  ,elt_types  ,elt_bytes ,elt_names( 1),  &
                   celidt    ,wrswch     ,ierr2     ,ittrkc        )

      if (ierr2   /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 3: element 1 'xyztrk'
!-----------------------------------------------------------------------
!
      do it=1,ntrk
         if (xa(it).ne.-999.999) then
            xyztrk(1,it)=xa(it)
            xyztrk(2,it)=ya(it)
            xyztrk(3,it)=za(it)
         else
            xyztrk(1,it) = default
            xyztrk(2,it) = default
            xyztrk(3,it) = default
         endif
      enddo
!
!     assign defaults to not yet released particles
!
      do it=ntrk+1,npmax
         xyztrk(1,it) = default
         xyztrk(2,it) = default
         xyztrk(3,it) = default
      enddo
      call putget (filnam        ,grnam3       ,nelmx3       ,elt_names( 2:),   &
                   elt_dims(:,2:),elt_types(2:),elt_bytes(2:),elt_names( 2 ),   &
                   celidt        ,wrswch       ,ierr2        ,xyztrk       )

      if (ierr2   /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 3, element 2 'wpart'
!-----------------------------------------------------------------------
      call putget (filnam        ,grnam3       ,nelmx3       ,elt_names( 2:),  &
                   elt_dims(:,2:),elt_types(2:),elt_bytes(2:),elt_names( 3 ),  &
                   celidt        ,wrswch       ,ierr2        ,wpart        )

      if (ierr2   /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 4, element 1 'track'
!-----------------------------------------------------------------------
      call putget (filnam        ,grnam4        ,nelmx4        ,elt_names( 4:),  &
                   elt_dims(:,4:),elt_types(4:) ,elt_bytes(4:) ,elt_names( 4 ),  &
                   1             ,wrswch        ,ierr2         ,track        )

      if (ierr2   /=  0) goto 999
!-----------------------------------------------------------------------
!-----write error message if error occured and set fout = .true.
!     the files will be closed in clsnef (called in triend)
!-----------------------------------------------------------------------
  999 continue
!
      if (ierr2   /=  0) then
         write(errmsg,'(i6)') ierr2
         write(lundia,'(a)') errmsg
         fout  = .true.
      endif

!-----------------------------------------------------------------------

      if ( timon ) call timstop ( ithndl )
      return
      end subroutine

end module m_wrttrk
