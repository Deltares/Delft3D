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

module dlpr12_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use m_waq_precision     ! single and double precision
      use timers
use putget_mod    ! generic procedure for putget routines
!
!  module procedure(s)
!
use delete_file_mod  ! explicit interface
use genfil_mod       ! explicit interface
use filldm_mod       ! explicit interface
!
implicit none     ! force explicit typing
!
contains
      subroutine dlpr12 ( iout   , lunout    , idump     , conc     , itime     ,   &
                          idt    , ihstrt    , ihstop    , ihstep   , dname     ,   &
                          sname  , mname     , nodump    , nosubs   , nolay     ,   &
                          ihflag , elt_names , elt_types , elt_dims , elt_bytes ,   &
                          rbuffr )
!
!     Deltares
!
!     created             : january 1993 by r.j. vos
!
!     operating system     compiler
!     ----------------     --------
!     dos 5.0              msf 5.10
!
!     function            : writes history results to filnam in
!                           delpar for postprocessing.
!
!     logical unitnumbers : iout = number of history output file
!
!     subroutines called  : filldm,
!                           putget.
!                           putget_chars.
!
!     parameters          :
!
!     name    kind     length     funct.  description
!     ----    -----    ------     ------- -----------
!     iout    integer     1       input   unit number output file name
!     lunout  integer     1       input   unit number monitoring file name
!     filnam  char*(*)    1       input   output file name
!                                         (without extension!!!)
!     idump   integer  nodump     input   segment numbers for dump
!     conc    real   notot*noseg  input   concentration values
!     itime   integer     1       input   present time in clock units
!     idt     integer     1       input   time step of simulation
!     ihstrt  integer     1       input   start time of history
!     ihstop  integer     1       input   stop time of history
!     ihstep  integer     1       input   time step of history
!     dname   char*20   nodump    input   names of monitoring stations
!     sname   char*20   notot     input   names of substances
!     mname   char*40     4       input   model identification
!     nodump  integer     1       input   amount of dump segments
!     notot   integer     1       input   total number of systems
!     ihflag  logical     1       output  true if history took place
!     elmnam  char*(*)    1       local   name of element, who's values
!                                         must be written or read
!     grnam1  char*16     1       local   header group name 1 (run id, text)
!     grnam2  char*16     1       local   header group name 1 (array dim's)
!     celid1  integer     1       local   index of cell to write to
!     celid2  integer     1       local   index of cell to write to
!     wrswch  logical     1       local   .true.: write to file
!
!     declare putget help var's
!

      implicit none

      integer   (int_wp )    , dimension(:)      :: idump
      real      (sp)    , dimension(:)      :: rbuffr
      real      (sp)    , dimension(:,:,:)  :: conc
      character (len= 6), parameter         :: subnam = 'dlpr12'
      character (len=20), dimension(:)      :: dname
      logical                               :: ihflag
!
!     variable included to keep up with crdefd.inc:
!
!     variables included to keep up with differences delwaq/delpar-crdefd.inc:
!
      character (len=40), dimension(:) :: mname
      character (len=20), dimension(:) :: sname
!
!     variables to be saved outside delpar-crdefd.inc:
!
      save type, itoff, wrswch
!
      character (len=256) filnam
      character (len=20)  type
!
!     declarations for in order to use putget
!
      integer(int_wp ), parameter  :: itofmx =    7
!
      character(len=20) :: substance
      character(len=16) :: grnam1, grnam2
      logical           :: wrswch=.true.,first1=.true.,first2=.true., nefis=.true.

      character(len=*) , pointer, dimension(:) :: elt_names, elt_types

      integer(int_wp ), pointer, dimension(:,:)     :: elt_dims
      integer(int_wp ), pointer, dimension(:)       :: elt_bytes

      integer(int_wp ), dimension(itofmx)  :: itoff
      real   (sp), dimension(4)       :: window
      integer(int_wp ), dimension(6)       :: nosize
!
      save          grnam1, grnam2,                           &
                    nosize, filnam, first1, first2, nefis ,   &
                    celid1, celid2
!
!     local scalars
!
      integer(int_wp ) :: idt      , ihstep , ihstop , ihstrt , iout   , itime
      integer(int_wp ) :: lunout   , nodump , nosubs , nolay  , noparm , notot
      integer(int_wp ) :: i        , ierr   , ierrem , indx   , j      , isub   , ilay
      integer(int_wp ) :: k        , nelmax , kk
!
!     r.j. vos , following 3 statements copied from code
!     data
!
      integer(int_wp ) ::  celid1 = 1
      integer(int_wp ) ::  celid2 = 1
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "dlpr12", ithndl )
!
      noparm    = 8
      elt_names(1) = 'TYPE'
      elt_names(2) = 'TITLE'
      elt_names(3) = 'SUBST_NAMES'
      elt_names(4) = 'LOCATION_NAMES'
      elt_names(5) = 'SIZES'
      elt_names(6) = 'PLOT_WINDOW'
      elt_names(7) = 'TIME_OFFSET'
      elt_names(8) = 'TIME'
      elt_types(1) = 'CHARACTER'
      elt_types(2) = 'CHARACTER'
      elt_types(3) = 'CHARACTER'
      elt_types(4) = 'CHARACTER'
      elt_types(5) = 'INTEGER'
      elt_types(6) = 'REAL'
      elt_types(7) = 'INTEGER'
      elt_types(8) = 'INTEGER'
      elt_bytes(1) =  20
      elt_bytes(2) =  40
      elt_bytes(3) =  20
      elt_bytes(4) =  20
      elt_bytes(5) =   4
      elt_bytes(6) =   4
      elt_bytes(7) =   4
      elt_bytes(8) =   4
!
      notot = nosubs * nolay
      do 1, i = 1, notot
         write (substance, '(a,i3.3)') 'SUBST_',i
         elt_names(i + noparm) = substance
         elt_types(i + noparm) = 'REAL'
         elt_bytes(i + noparm) = 4
    1 continue
!
!
      if (first1) then
!
!       adapt dimensions
!
        nelmax = noparm + notot
!
!       first inquire file name (via monitoring file)
!
        inquire(unit = lunout, name = filnam)
!
!       generate file name for history file
!
        call genfil(filnam,'his-',indx)
        call delete_file(filnam(:indx)//'.dat', ierr )
        call delete_file(filnam(:indx)//'.def', ierr )
    3   first1=.false.
      endif
!
      do 5 i = 1, 4
         window(i) = 0.0
5     continue
!
!     initialize model dependent names for nefis
!
      if (first2) then
        first2 = .false.
!
!       group names etc.
!
        grnam1 = 'DELPAR_PARAMS'
        grnam2 = 'DELPAR_RESULTS'
        type   = 'HIS-FILE[PART]'
!
!       time off-set
!
        itoff(     1) = 0
        itoff(     2) = 0
        itoff(     3) = 0
        itoff(     4) = ihstrt
        itoff(     5) = ihstop
        itoff(     6) = ihstep
        itoff(itofmx) = 0
!
!       initialize sizes; 1 - notot
!                         2 - noseg
!                         3 - nodmp (0 for .map)
!                         4 - nolay
!                         5 - nocol (.plo)
!                         6 - norow (.plo)
!
!
        nosize(1) = notot
        nosize(2) = 0
        nosize(3) = nodump
        nosize(4) = 0
        nosize(5) = 0
        nosize(6) = 0
!
!       set up the element dimensions
!
!       group 1
!
        call filldm (elt_dims,1   ,1   ,1     ,0    ,0     ,0     ,0    )
        call filldm (elt_dims,2   ,1   ,4     ,0    ,0     ,0     ,0    )
        call filldm (elt_dims,3   ,1   ,notot ,0    ,0     ,0     ,0    )
        call filldm (elt_dims,4   ,1   ,nodump,0    ,0     ,0     ,0    )
        call filldm (elt_dims,5   ,1   ,6     ,0    ,0     ,0     ,0    )
        call filldm (elt_dims,6   ,1   ,4     ,0    ,0     ,0     ,0    )
        call filldm (elt_dims,7   ,1   ,itofmx,0    ,0     ,0     ,0    )
!
!       group 2
!
        call filldm (elt_dims,noparm,1       ,1    ,0    ,0     ,0     ,  0    )
        do 100, i = 1, notot
          call filldm (elt_dims,noparm+i  ,1  ,nodump ,0  ,  0  ,0   ,0     )
  100   continue
      endif
!
      ihflag = .false.
      if ( ihstep                    <=  0      ) goto 9999
      if ( itime                     < ihstrt   ) goto 9999
      if ( itime-idt                 >=  ihstop ) goto 9999
      if ( mod(itime-ihstrt,ihstep)  >=  idt    ) goto 9999
      ihflag = .true.
!
!     initialise the history file
!
      ierrem = 0
      if ( itime-ihstrt  < idt ) then
!
        write (iout) ( mname(k) , k = 1,4 )
        write (iout) notot    , nodump
        write (iout) ( sname(k) , k = 1,notot )
        write (iout) ( idump(k),dname(k), k = 1,nodump )
!
        if (nefis) then
!
!         write all elements to file; all definition and creation of files,
!         data groups, cells and elements is handled by putget.
!

          call putget      (filnam    , grnam1    , noparm-1     , elt_names , elt_dims  ,   &
                            elt_types , elt_bytes , elt_names(1) , celid1    , wrswch    ,   &
                            ierr      , type    )
          if (ierr  /=  0) go to 110
!
          call putget      (filnam     , grnam1     , noparm-1    , elt_names , elt_dims  ,  &
                            elt_types  , elt_bytes  , elt_names(2), celid1    , wrswch    ,  &
                            ierr       , mname   )
          if (ierr  /=  0) go to 110
          call putget      (filnam     , grnam1     , noparm-1    , elt_names , elt_dims  ,  &
                            elt_types  , elt_bytes  , elt_names(3), celid1    , wrswch    ,  &
                            ierr       , sname   )
          if (ierr  /=  0) goto  110
          call putget      (filnam     , grnam1     , noparm-1    , elt_names , elt_dims  ,  &
                            elt_types  , elt_bytes  , elt_names(4), celid1    , wrswch    ,  &
                            ierr       , dname   )
          if (ierr  /=  0) goto  110
          call putget     (filnam     , grnam1     , noparm-1    , elt_names , elt_dims  ,   &
                           elt_types  , elt_bytes  , elt_names(5), celid1    , wrswch    ,   &
                           ierr       , nosize  )
          if (ierr  /=  0) goto  110
          call putget      (filnam     , grnam1     , noparm-1    , elt_names , elt_dims  ,  &
                            elt_types  , elt_bytes  , elt_names(6), celid1    , wrswch    ,  &
                            ierr       , window  )
          if (ierr  /=  0) goto  110
          call putget     (filnam     , grnam1     , noparm-1    , elt_names , elt_dims  ,   &
                           elt_types  , elt_bytes  , elt_names(7), celid1    , wrswch    ,   &
                           ierr       , itoff   )
          if (ierr  /=  0) go to 110
          goto 120
  110     ierrem = ierr
        endif
      endif
!
  120 continue
!
!     dump a history
!
      write (iout) itime , (((conc(isub,ilay,j),isub=1,nosubs),ilay=1,nolay)  ,j=1,nodump)
!
      if (nefis) then
!
!       produce a his record for nefis
!
        if (ierrem == 0) then
!
!         no previous nefis errors
!
          itoff(itofmx) = celid1
          call putget     (filnam     , grnam1     , noparm-1    , elt_names , elt_dims  ,  &
                           elt_types  , elt_bytes  , elt_names(7), 1         , wrswch    ,  &
                           ierr    ,                               itoff   )
          if (ierr  /=  0) go to 310
!
!         write all elements to file; all definition and creation of files,
!         data groups, cells and elements is handled by putget.
!

          call putget     (filnam             , grnam2              , notot + 1         , &
                           elt_names(noparm:) , elt_dims(:,noparm:) , elt_types(noparm:), &
                           elt_bytes(noparm:) , elt_names(noparm)   , celid1            , &
                           wrswch             , ierr                , itime     )
          if  (ierr  /=  0) go to 310
          celid1 = celid1 + 1
!
!         do for all layers
!
          kk=1
          do ilay=1,nolay

!         do for all substances
!
             do isub=1,nosubs
!
!              do for all locations
!
                 do  j = 1, nodump
                   rbuffr(j) = conc(isub,ilay,j)
                 end do
                 call putget (filnam              , grnam2               , notot + 1  ,  &
                              elt_names(noparm:)  , elt_dims(:,noparm:)  ,               &
                              elt_types(noparm:)  , elt_bytes(noparm:)   ,               &
                              elt_names(noparm+kk), celid2               , wrswch     ,  &
                              ierr                , rbuffr   )
!                 write(lunout,'(a,i6,4x,a20/(8(1pe14.5)))') &
!                   'dlpr12 : ',itime,  sname(kk) ,(rbuffr(j),j=1,nodump)
                 kk = kk + 1
                 if (ierr  /=  0) go to 310
             end do
          end do
          celid2 = celid2 + 1
          goto 9999
  310     ierrem = ierr
        endif
!
        if (ierrem  /=  0) then
!
!         echo error to logging file
!
          nefis = .false.
          write (lunout, 99003)ierrem,subnam,                            &
                             itime /86400, mod(itime ,86400)/3600,       &
                             mod(itime ,3600)/60, mod(itime ,60)

        endif
      endif
!
 9999 if ( timon ) call timstop ( ithndl )
      return
!
!     formats
!
99003 format( ' Error 4802. Writing NEFIS file', i10, '; in ', a6,       &
               ' at simulation time :'           &
               ,i3,'d ',i2,'h ',i2,'m ',i2,'s!')

!
      end subroutine
end module
