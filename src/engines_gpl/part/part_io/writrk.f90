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

module writrk_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use m_waq_precision       ! single and double precision
use timers
!
!  module procedure(s)
!
use putget_mod      ! explicit interface
use filldm_mod      ! explicit interface
!
implicit none       ! force explicit typing
!
contains
      subroutine writrk(lundia    ,fout      ,filnam    ,ntrk      ,  &
                        tstring   ,dt        ,nstep     ,ibuff     ,  &
                        rbuff     ,cbuff     ,track     ,nopmax   )
!-----------------------------------------------------------------------
!         Deltares
!
!             module: subroutine writrk
!           function: writes the initial group 1 ('trk-const') to
!                     particle tracking file
!
!                     derived from wridro drogue subroutine in flow
!                     adapted for particle tracking
!             date  : 30-09-2001
!
!        method used:
!               date: 18-07-2001
!         programmer: h.h. leepel/w. ter horst, a.j. mourits
!         cvs header
!            $author: mourits $
!              $date: 20-07-01 9:58 $
!            $source: /u/trisula/cvsroot/trisula/output/writrk.f,v $
!          $revision: 19 $
!-----------------------------------------------------------------------
!   calling routines:              inippr
!-----------------------------------------------------------------------
!   called  routines:              filldm
!                                  putgtr
!                                  putgti
!                                  putgch (*)
!   (*)  putgch is the (renamed) flow routine putget_chars
!        putget_chars is the part routine for reading/writing characters
!-----------------------------------------------------------------------
!  formal parameters:
!  ------------------
!
!   var. i/o  type dimensions
!   -------------------------
!
! dt      i   r*4                  integration time step [in min.]
! cbuff   o   i*4  2,ntrk          help array for writing nefis files
! fout    o   l*4                  flag=true if an error is encountered
! ibuff   o   i*4  2,ntrk          help array for writing nefis files
! itdate  i   i*4                  reference date for the simulation
!                                  times. format: yyyymmdd
! lundia  io  i*4                  unit number for diagnostic file
! mnktrk  i   i*4  3,ntrk          m(1)n(2)k(3)-coordinate of track
!                                  starting points
! namtrk  i  ch*20 ntrk            array containing the names of the
!                                  tracks
! ntrk    i   i*4                  number of tracks
! rbuff   o   i*4  2,ntrk          help array for writing nefis files
! simdat  i  ch*16                 simulation date representing the
!                                  flow condition at this date
! track   r   r*4   8*ntrk         1: begin index  m of released part.
!                                  2: begin index  n of released part.
!                                  3: begin index  k of released part.
!                                  4: begin coord. x of released part.
!                                  5: begin coord. y of released part.
!                                  6: begin coord. z of released part.
!                                  7: actual time step nr. when particle
!                                     was released
!                                  8: discharge location (dye/cont.) from
!                                     where particle was released
! filnam  i  ch*(*)                base name for tracking file (trk ....)
! tunit   i   r*4                  time scale for time parameters (60.)
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
! grnam1     ch*16                 data-group name defined for the
!                                  nefis-files
! idummy      i*4  1               help array to read/write nefis files
! ierror      i*4                  local error flag for nefis files
! ival        i*4  2               local array for writing itdate and
!                                  time (:= 00:00:00)
! elt_bytes      i*4  nelmx           array containing the number of by-
!                                  tes of each single elt_types
! rdummy      r*4  1               help array to read/write nefis files
! wrswch      l*4                  flag to write file
!                                    .true. : write to  file
!                                    .false.: read from file
!-----------------------------------------------------------------------
!
!
!-declaration and specifications
!
!
      integer(int_wp ), parameter              :: nelmxx = 11
!
      integer(int_wp )                         :: ntrk  ,ittrkf,ittrki,itdate,lundia
      integer(int_wp )                         :: ierror,id
      integer(int_wp )                         :: celidt
!
      real                                :: dt    ,tunit

      real, dimension(:,:)                :: track
!
      character (len=*), pointer, dimension(:)      :: cbuff
      character (len=6)                   :: errmsg
      character (len=16)                  :: simdat
      character (len=256)                 :: filnam
      character (len=20)                  :: tstring
      character (len=80)                  :: formt
!
      logical                             :: wrswch
!
      integer(int_wp ), pointer, dimension(:,:       )  :: ibuff
      integer(int_wp ), dimension(2)           :: ival
      real   (sp), pointer, dimension(:,:       )  :: rbuff
!
      character (len=16),dimension(1)     :: cdum16
!
      save          grnam0 ,grnam1 ,elt_names,elt_types ,elt_bytes ,celidt
!
!-----nefis statics
!
      save          first
      integer(int_wp ), dimension(6,nelmxx)   :: elt_dims
      logical                            :: first   = .true.
      real(sp)                           :: default = 999.999
!
!-----end nefis statics
!
      character (len=16) :: grnam0  = 'fileinfo'
      character (len=16) :: grnam1  = 'trk-const'
      character (len=16), dimension(nelmxx) :: elt_names  =               &
                       (/ 'FILETYPE  ',    &
                          'DIOVERSION',    &
                          'ITDATE    ',    &
                          'TUNIT     ',    &
                          'DT        ',    &
                          'SIMDAT    ',    &
                          'NTRK      ',    &
                          'NAMTRK    ',    &
                          'MNKTRK    ',    &
                          'DXYZTRK   ',    &
                          'NTTRK     ' /)
      character (len=16), dimension(nelmxx) :: elt_types  =               &
                       (/ 'CHARACTER',      &
                          'CHARACTER',      &
                          'INTEGER  ',      &
                          'REAL     ',      &
                          'REAL     ',      &
                          'CHARACTER',      &
                          'INTEGER  ',      &
                          'CHARACTER',      &
                          'INTEGER  ',      &
                          'REAL     ',      &
                          'INTEGER  ' /)
      integer(int_wp ),  dimension(nelmxx) :: elt_bytes = &
                    (/ 20 , 20, 4 , 4 , 4 , 16, 4, 20, 4 , 4 , 4 /)
!
!    local scalar
!
      integer(int_wp ) :: ih     , im     , imonth , int   , is     , it
      integer(int_wp ) :: ittrkb , ittrke , iyear  , ndig  , nelmx  , nopmax , nstep
      logical     :: fout
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "writrk", ithndl )

!-----------------------------------------------------------------------
!-----initialisation
!-----------------------------------------------------------------------
      ierror = 0
      celidt = 1
      errmsg = ' '
      wrswch = .true.
!----------------------------------------------------------------------
!-----set up the element dimensions
!----------------------------------------------------------------------
      if (first ) then
         first  = .false.
!        group 0     : version control
         call filldm (elt_dims,1   ,1  ,1     ,0     ,0     ,0     ,0  )
         call filldm (elt_dims,2   ,1  ,1     ,0     ,0     ,0     ,0  )
!        group 1     : initialize
         call filldm (elt_dims,3   ,1  ,2     ,0     ,0     ,0     ,0  )
         call filldm (elt_dims,4   ,1  ,1     ,0     ,0     ,0     ,0  )
         call filldm (elt_dims,5   ,1  ,1     ,0     ,0     ,0     ,0  )
         call filldm (elt_dims,6   ,1  ,1     ,0     ,0     ,0     ,0  )
         call filldm (elt_dims,7   ,1  ,1     ,0     ,0     ,0     ,0  )
         call filldm (elt_dims,8   ,1  ,nopmax,0     ,0     ,0     ,0  )
         call filldm (elt_dims,9   ,2  ,3     ,nopmax,0     ,0     ,0  )
         call filldm (elt_dims,10  ,2  ,3     ,nopmax,0     ,0     ,0  )
         call filldm (elt_dims,11  ,2  ,2     ,nopmax,0     ,0     ,0  )
      endif

!-----------------------------------------------------------------------
!-----group 0, element 'file version'
!-----------------------------------------------------------------------
      nelmx = 2
      cbuff(1)='part tracking file'
      call putget  (filnam    ,grnam0    ,nelmx     ,elt_names    ,   &
                    elt_dims  ,elt_types ,elt_bytes ,elt_names( 1),   &
                    celidt    ,wrswch    ,ierror    ,cbuff    )

      if (ierror  /=  0) goto 999

!-----------------------------------------------------------------------
!-----group 0, element 'delft io version'
!-----------------------------------------------------------------------

      cbuff(1)='1.00.00'
      call putget   (filnam    ,grnam0     ,nelmx      ,elt_names    ,   &
                     elt_dims  ,elt_types  ,elt_bytes  ,elt_names( 2),   &
                     celidt    ,wrswch     ,ierror     ,cbuff (1)  )

      if (ierror  /=  0) goto 999

!-----------------------------------------------------------------------
!-----group 1, element 'itdate'
!-----------------------------------------------------------------------
!     write(title(4),'(a3,1x,i4.4,5(1x,i2.2) )')
!    *       'T0:',iyear,imonth,id,ih,im,is
!
      nelmx = 9
      read(tstring,'(4x,i4,5(1x,i2))') iyear,imonth,id,ih,im,is
      itdate = iyear*10000 + imonth*100 + id
      ival(1)= itdate
      ival(2)= 000000
      call putget (filnam         ,grnam1       ,nelmx        ,elt_names(3:) ,  &
                   elt_dims(1:,3:),elt_types(3:),elt_bytes(3:),elt_names(3 ) ,  &
                   celidt         ,wrswch       ,ierror       ,ival      )
      if (ierror  /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 1, element 'tunit'
!-----------------------------------------------------------------------
      tunit = 1.0
      call putget (filnam         ,grnam1        ,nelmx         ,elt_names(3:) ,  &
                   elt_dims(1:,3:),elt_types(3:) ,elt_bytes(3:) ,elt_names(4)  ,  &
                   celidt         ,wrswch        ,ierror        ,tunit   )
      if (ierror  /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 1, element 'dt'
!-----------------------------------------------------------------------
      call putget (filnam       ,grnam1    ,nelmx      ,elt_names(3:),  &
                   elt_dims(1:,3:),elt_types(3:),elt_bytes(3:) ,elt_names(5) ,  &
                   celidt       ,wrswch    ,ierror     ,dt       )

      if (ierror  /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 1, element 'simdat'
!-----------------------------------------------------------------------
!     simdat( 1:16) = 'yyyymmdd  hhmmss'
!
      write(simdat,'(i4.4,2i2.2,2x,3i2.2)') iyear,imonth,id,ih,im,is
      cdum16(1) = simdat
      call putget (filnam         ,grnam1       ,nelmx        ,elt_names(3:) ,  &
                   elt_dims(1:,3:),elt_types(3:),elt_bytes(3:),elt_names(6 ) ,  &
                   celidt         ,wrswch       ,ierror       ,simdat    )

      if (ierror  /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 1, element 'ntrk'
!-----------------------------------------------------------------------
      call putget (filnam         ,grnam1       ,nelmx         ,elt_names(3:) ,  &
                   elt_dims(1:,3:),elt_types(3:),elt_bytes(3:) ,elt_names(7 ) ,  &
                   celidt         ,wrswch       ,ierror        ,nopmax   )

      if (ierror  /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 1, element 'namtrk'
!-----------------------------------------------------------------------
!
!     generate names for particles tracks
!
      ndig = nint( alog10(real(nopmax)) ) + 1
      write(formt,'(a,i8.8,a,i8.8,a)') '(a,i',ndig,'.',ndig,')'
      do it=1,nopmax
         write(cbuff(it),formt) 'particle_',it
      enddo
!
      call putget (filnam         ,grnam1       ,nelmx        ,elt_names(3:) , &
                   elt_dims(1:,3:),elt_types(3:),elt_bytes(3:),elt_names(8)  , &
                   celidt         ,wrswch       ,ierror       ,cbuff   )

      if (ierror  /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 1, element 'mnktrk'
!              because the coordinates used to calculate the track
!              tracks with are different from the coordinates the user
!              defines (right upper corner versus left lower corner
!              of a gridcell) the ibuff array is used
!-----------------------------------------------------------------------
!
!     store (m,n,k) coordinates of cell from where particles
!     were released
!
      do it = 1,ntrk
         ibuff (1,it) = nint(track(1,it))
         ibuff (2,it) = nint(track(2,it))
         ibuff (3,it) = nint(track(3,it))
      enddo
!
!     assign defaults for not yet released particles
!
      do it = ntrk+1,nopmax
         ibuff (1,it) = int(default)
         ibuff (2,it) = int(default)
         ibuff (3,it) = int(default)
      enddo
!
      call putget (filnam         ,grnam1       ,nelmx        ,elt_names(3:) , &
                   elt_dims(1:,3:),elt_types(3:),elt_bytes(3:),elt_names(9 ) , &
                   celidt         ,wrswch       ,ierror       ,ibuff     )

      if (ierror  /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 1, element 'dxyztrk'
!-----------------------------------------------------------------------
!
!     store (x,y,z) coordinates of release location
!
      do it=1,ntrk
         rbuff(1,it) = track(4,it)
         rbuff(2,it) = track(5,it)
         rbuff(3,it) = track(6,it)
      enddo
!
!     assign defaults for not yet released particles
!
      do it=ntrk+1,nopmax
         rbuff(1,it) = default
         rbuff(2,it) = default
         rbuff(3,it) = default
      enddo
!
      call putget (filnam         ,grnam1       ,nelmx        ,elt_names(3:) , &
                   elt_dims(1:,3:),elt_types(3:),elt_bytes(3:),elt_names(10) , &
                   celidt         ,wrswch       ,ierror       ,rbuff  )
      if (ierror  /=  0) goto 999
!-----------------------------------------------------------------------
!-----group 1, element 'nttrk'
!              time frame relative to start time step (ittrkf) and time
!              step interval (ittrki). the ibuff array is used
!-----------------------------------------------------------------------
!
!     output to tracking file
!           - from start of simulation
!           - each time step
!           - till end of simulation
!
!           track(7,i) : time step number of release
!
      ittrkf = 0
      ittrki = 1
 !     ittrkb = int(track(7,it))
      ittrke = nstep
      do it = 1,ntrk
         ittrkb = int(track(7,it))
         ibuff (1,it) = nint((ittrkb-ittrkf+1.01*ittrki)/ittrki)
         ibuff (2,it) = nint((ittrke-ittrkf+1.01*ittrki)/ittrki)
      enddo
!
!     assign defaults for not yet released particles
!
      do it = ntrk+1,nopmax
         ibuff (1,it) = int(default)
         ibuff (2,it) = int(default)
      enddo
      call putget (filnam         ,grnam1       ,nelmx        ,elt_names(3:) , &
                   elt_dims(1:,3:),elt_types(3:),elt_bytes(3:),elt_names(11) , &
                   celidt         ,wrswch       ,ierror       ,ibuff     )

      if (ierror  /=  0) goto 999
!-----------------------------------------------------------------------
!-----write error message if error occured and set fout = .true.
!     the files will be closed in clsnef (called in triend)
!-----------------------------------------------------------------------
  999 continue
!
      if (ierror  /=  0) then
         write(errmsg,'(i6)') ierror
         write(lundia,'(a)') errmsg
         fout  = .true.
      endif
!-----------------------------------------------------------------------

      if ( timon ) call timstop ( ithndl )
      return
      end subroutine
end module

