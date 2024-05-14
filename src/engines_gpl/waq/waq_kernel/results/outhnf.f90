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
      module m_outhnf
      use m_waq_precision


      implicit none

      contains


      subroutine outhnf ( iout  , lchout, itime , moname, noseg , & 
                         notot1, conc1 , syname, notot2, conc2 , & 
                         iostrt, iostop, iostep, nodump, idump , & 
                         duname, rbuffr, init  )

!     Deltares        sector waterresources and environment
!
!     created            : june 1988  by l. postma (dlwq12)
!     modified           : june 1992 by m. zeeuw (nefis incorporation)
!     modified           : aug. 1993 by jan van beek (outhnf)
!     modified           : dec  2007 by jan van beek (allocatable arrays)
!
!     function           : gives his output to nefis files
!                          conc1 is map of tatal area
!                          conc2 is already mapped on monitor points
!
!     subroutines called : delete_file, deletes a file
!                          fill_element_dimensions, fills elements dimension array
!                          manage_nefis_data_character, handles i/o to nefis file for char's
!                          putget, handles i/o to nefis file for int/real
!

      use m_logger, only : terminate_execution, get_log_unit_number
      use timers
      use nefis_data, only : manage_nefis_data_character
      use m_array_manipulation, only : fill_element_dimensions
      use data_processing, only : delete_file

      implicit none

!     declaration of arguments

      integer(kind=int_wp), intent(in)     ::iout                   ! unit number output file
      integer(kind=int_wp), intent(in)     ::itime                  ! present time in clock units
      integer(kind=int_wp), intent(in)     ::noseg                  ! total number of segments
      integer(kind=int_wp), intent(in)     ::notot1                 ! total number of systems
      integer(kind=int_wp), intent(in)     ::notot2                 ! number of vars in conc2
      integer(kind=int_wp), intent(in)     ::iostrt                 ! start time of output
      integer(kind=int_wp), intent(in)     ::iostop                 ! stop time of output
      integer(kind=int_wp), intent(in)     ::iostep                 ! time step of output
      integer(kind=int_wp), intent(in)     ::nodump                 ! number of monitor points
      integer(kind=int_wp), intent(inout)  ::init                   ! init flag (1=yes,!1=no)
      integer(kind=int_wp), intent(in)     ::idump(nodump)          ! segment number of monitor points
      real(kind=real_wp), intent(in)     ::conc1(notot1,noseg)    ! concentration values
      real(kind=real_wp), intent(in)     ::conc2(notot2,nodump)   ! concentration values array 2
      real(kind=real_wp), intent(out)    ::rbuffr(nodump)         ! output buffer
      character(len=*)     , intent(in)    :: lchout                 ! name output file
      character(len=40)    , intent(in)    :: moname(4)              ! model identhification
      character(len=*)     , intent(in)    :: syname(notot1+notot2)  ! names of substances + extra
      character(len=*)     , intent(in)    :: duname(nodump)         ! name of monitor points

!     local variables

      logical                  , parameter :: lwrite = .true.        ! .true.: write to file
      logical                  , parameter :: lread  = .false.
      integer(kind=int_wp), parameter  ::noelm1 = 7             ! number of elements in group 1
      integer(kind=int_wp), parameter  ::noparm = noelm1 + 1    ! fixed number of elements in file

      integer(kind=int_wp) ::nelmxx                 ! total number of elements
      character(len=255)            , save :: defnam                 ! filename nefis definition file
      character(len=255)            , save :: datnam                 ! filename nefis data file
      character(len=132)                   :: error_string
      character(len=20)                    :: type
      integer(kind=int_wp), save  ::celid1 = 1             ! index of cell group 2
      integer(kind=int_wp), save  ::celid2 = 1             ! index of cell group 1
      integer(kind=int_wp) ::noelm2                 ! number of elements in group 2
      logical                       , save :: nefis  = .true.
      integer(kind=int_wp) ::nosize(6)
      real(kind=real_wp) ::window(4)
      integer(kind=int_wp), save  ::itoff (7)
      character(len=16)             , save :: grnam1                 ! group 1 name (runid,text,dim's)
      character(len=16)             , save :: grnam2                 ! group 2 name (time dep data)
      character(len=16), allocatable, save :: elmnms(:)              ! name of elements on file
      character(len=16), allocatable, save :: elmpts(:)              ! element types
      integer(kind=int_wp), allocatable, save  ::elmdms(:,:)            ! element dimensions
      integer(kind=int_wp), allocatable, save  ::nbytsg(:)              ! element number of bytes
      integer(kind=int_wp) ::ierr                   ! error indication
      integer(kind=int_wp) ::ierrem                 ! error indication
      integer(kind=int_wp) ::ierr_alloc             ! error indication allocation
      integer(kind=int_wp) ::iret_error             ! error indication nefis
      integer(kind=int_wp) ::lunout                 ! unit number report file
      integer(kind=int_wp) ::i                      ! loop counter
      integer(kind=int_wp) ::isys                   ! loop counter substances
      integer(kind=int_wp) ::isys2                  ! index in second conc array
      integer(kind=int_wp) ::iseg                   ! loop counter segments
      integer(kind=int_wp) ::neferr                 ! nefis error function
      integer(kind=int_wp) ::notot                  ! total number of output variables

      integer(kind=int_wp), save                         ::fd_nef = -1            ! handle to NEFIS file
      integer(kind=int_wp) , external                    :: FLSDAT, FLSDEF

      integer(kind=int_wp) ::ithandl = 0
      if ( timon ) call timstrt ( "outhnf", ithandl )

!     some init

      notot  = notot1 + notot2
      noelm2 = notot  + 1
      ierrem = 0
      call get_log_unit_number(lunout)

!     initialize file

      if ( init == 1 ) then
         init = 0

         ! allocate arrays

         nelmxx = noparm + notot
         if (allocated(elmnms)) deallocate(elmnms)
         if (allocated(elmpts)) deallocate(elmpts)
         if (allocated(elmdms)) deallocate(elmdms)
         if (allocated(nbytsg)) deallocate(nbytsg)

         allocate(elmnms(nelmxx),elmpts(nelmxx),elmdms(6,nelmxx),nbytsg(nelmxx),stat=ierr_alloc)
         if ( ierr_alloc /= 0 ) then
            write(lunout,*) 'ERROR : allocating nefis output structure'
            write(*,*) 'ERROR : allocating nefis output structure'
            call terminate_execution(1)
         endif

         ! initialize independent element names

         elmnms(1)='TYPE'          ; elmpts(1)='CHARACTER'; nbytsg(1)=20
         elmnms(2)='TITLE'         ; elmpts(2)='CHARACTER'; nbytsg(2)=40
         elmnms(3)='SUBST_NAMES'   ; elmpts(3)='CHARACTER'; nbytsg(3)=20
         elmnms(4)='LOCATION_NAMES'; elmpts(4)='CHARACTER'; nbytsg(4)=20
         elmnms(5)='SIZES'         ; elmpts(5)='INTEGER  '; nbytsg(5)= 4
         elmnms(6)='PLOT_WINDOW'   ; elmpts(6)='REAL     '; nbytsg(6)= 4
         elmnms(7)='TIME_OFFSET'   ; elmpts(7)='INTEGER  '; nbytsg(7)= 4
         elmnms(8)='TIME'          ; elmpts(8)='INTEGER  '; nbytsg(8)= 4

         ! initialize dependent element names always SUBST_nnn

         do isys = 1, notot
            elmnms(isys + noparm) = 'SUBST_'
            write (elmnms(isys+noparm)(7:9),'(i3.3)') isys
            elmpts(isys + noparm) = 'REAL'
            nbytsg(isys + noparm) = 4
         enddo

         ! proces file name

         defnam = lchout
         do i = len(defnam), 1, -1
            if (defnam(i:i) == '.') then

               ! found filename separator, remove file-id

               defnam(i:) = ' '
               exit
            endif
         enddo
         datnam = defnam
         defnam = trim(defnam)//'.hdf'
         datnam = trim(datnam)//'.hda'

         ! delete existing nefis files

         call delete_file ( datnam, ierr )
         call delete_file ( defnam, ierr )

         ! initialize window

         window = 0.0

         ! group names etc.

         grnam1 = 'DELWAQ_PARAMS'
         grnam2 = 'DELWAQ_RESULTS'
         type   = 'HISTORY'

         ! time off-set

         itoff(     1) = 0
         itoff(     2) = 0
         itoff(     3) = 0
         itoff(     4) = iostrt
         itoff(     5) = iostop
         itoff(     6) = iostep
         itoff(     7) = 0

         ! initialize sizes; 1 - notot
         !                   2 - noseg
         !                   3 - nodmp (0 for .map)
         !                   4 - nolay
         !                   5 - nocol (.plo)
         !                   6 - norow (.plo)

         nosize(1) = notot
         nosize(2) = 0
         nosize(3) = nodump
         nosize(4) = 0
         nosize(5) = 0
         nosize(6) = 0

         ! set up the element dimensions

         ! group 1

         call fill_element_dimensions (elmdms,1   ,1   ,1     ,0     ,0    ,0     ,0    )
         call fill_element_dimensions (elmdms,2   ,1   ,4     ,0     ,0    ,0     ,0    )
         call fill_element_dimensions (elmdms,3   ,1   ,notot ,0     ,0    ,0     ,0    )
         call fill_element_dimensions (elmdms,4   ,1   ,nodump,0     ,0    ,0     ,0    )
         call fill_element_dimensions (elmdms,5   ,1   ,6     ,0     ,0    ,0     ,0    )
         call fill_element_dimensions (elmdms,6   ,1   ,4     ,0     ,0    ,0     ,0    )
         call fill_element_dimensions (elmdms,7   ,1   ,7     ,0     ,0    ,0     ,0    )

         ! group 2

         call fill_element_dimensions (elmdms,noparm,1       ,1    ,0    ,0     ,0     ,0    )
         do isys = 1, notot
            call fill_element_dimensions (elmdms,noparm+isys ,1    ,nodump,0     ,0     ,0     ,0     )
         enddo

         ! write all elements to file; all definition and creation of files,
         ! data groups, cells and elements is handled by putget.

         call manage_nefis_data_character(defnam, datnam, grnam1, noelm1   , elmnms, & 
                    elmdms, elmpts, nbytsg, elmnms(1), celid1, & 
                    lwrite, ierr  , type  , fd_nef)
         if (ierr /= 0) go to 110

         call manage_nefis_data_character(defnam, datnam, grnam1, noelm1   , elmnms, & 
                    elmdms, elmpts, nbytsg, elmnms(2), celid1, & 
                    lwrite, ierr  , moname, fd_nef)
         if (ierr /= 0) go to 110

         call manage_nefis_data_character(defnam, datnam, grnam1, noelm1   , elmnms, & 
                    elmdms, elmpts, nbytsg, elmnms(3), celid1, & 
                    lwrite, ierr  , syname, fd_nef)
         if (ierr /= 0) go to 110

         call manage_nefis_data_character(defnam, datnam, grnam1, noelm1   , elmnms, & 
                    elmdms, elmpts, nbytsg, elmnms(4), celid1, & 
                    lwrite, ierr  , duname, fd_nef)
         if (ierr /= 0) go to 110

         call putget(defnam, datnam, grnam1, noelm1   , elmnms, & 
                    elmdms, elmpts, nbytsg, elmnms(5), celid1, & 
                    lwrite, ierr  , nosize, fd_nef)
         if (ierr /= 0) go to 110

         call putget(defnam, datnam, grnam1, noelm1   , elmnms, & 
                    elmdms, elmpts, nbytsg, elmnms(6), celid1, & 
                    lwrite, ierr  , window, fd_nef)
         if (ierr /= 0) go to 110

         call putget(defnam, datnam, grnam1, noelm1   , elmnms, & 
                    elmdms, elmpts, nbytsg, elmnms(7), celid1, & 
                    lwrite, ierr  , itoff , fd_nef)

  110    continue
         ierrem = ierr
         if ( ierrem /= 0 ) nefis = .false.
      endif

      ! produce a map record for nefis

      if ( nefis ) then

         ! update number of cells (records) written

         itoff(7) = celid2
         call putget(defnam, datnam, grnam1, noelm1   , elmnms, & 
                    elmdms, elmpts, nbytsg, elmnms(7), celid1, & 
                    lwrite, ierr  , itoff , fd_nef)
         if (ierr /= 0) go to 310

         ! write actual time to cell

         call putget (defnam        , datnam          , & 
                     grnam2        , noelm2          , & 
                     elmnms(noparm), elmdms(1,noparm), & 
                     elmpts(noparm), nbytsg(noparm)  , & 
                     elmnms(noparm), celid2          , & 
                     lwrite        , ierr            , & 
                     itime         , fd_nef)
         if  (ierr /= 0) go to 310

         ! fill and write output buffer for every output variable to cell

         do isys = 1, notot

            if ( isys <= notot1 ) then

               do iseg = 1, nodump
                  rbuffr(iseg) = conc1(isys, idump(iseg))
               enddo

            else

               isys2 = isys - notot1
               do iseg = 1, nodump
                  rbuffr(iseg) = conc2(isys2,iseg)
               enddo

            endif

            ! write buffer

            call putget (defnam             , datnam          , & 
                        grnam2             , noelm2          , & 
                        elmnms(noparm)     , elmdms(1,noparm), & 
                        elmpts(noparm)     , nbytsg(noparm)  , & 
                        elmnms(noparm+isys), celid2          , & 
                        lwrite             , ierr            , & 
                        rbuffr             , fd_nef)
            if  (ierr /= 0) go to 310
         enddo

         celid2 = celid2 + 1

  310    continue
         ierrem = ierr

      endif

      if (ierrem /= 0) then

         ! echo error to logging file

         write (lunout, 2000) ierrem
         iret_error = neferr(1, error_string)
         write (lunout, *) iret_error,':',error_string
      endif

      ierr = FLSDAT( fd_nef )
      ierr = FLSDEF( fd_nef )

      if ( timon ) call timstop ( ithandl )
      return

 2000 format ( 'ERROR writing NEFIS history file errno:', I7 )

      end

      end module m_outhnf
