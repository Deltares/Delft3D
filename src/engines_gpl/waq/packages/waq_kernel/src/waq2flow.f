!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
      module m_waq2flow

      implicit none

      contains


      subroutine waq2flow(nrvart, ounam , ipoint, nocons, nopa  ,
     +                    nofun , nosfun, notot , conc  , segfun,
     +                    func  , param , cons  , idt   , itime ,
     +                    volume, noseg , nosys , nodump, idump ,
     +                    nx    , ny    , lgrid , bound , noloc ,
     +                    proloc, nodef , defaul, lunrep)

      use m_fioutv
      use m_zoek
      use m_srstop
      use m_monsys
      use m_gkwini
      use m_getcom
      use m_dio_plt

      implicit none


      ! arguments

      integer                 :: nrvart
      character(len=20)       :: ounam(nrvart)
      integer                 :: ipoint(nrvart)
      integer                 :: nocons
      integer                 :: nopa
      integer                 :: nofun
      integer                 :: nosfun
      integer                 :: notot
      integer                 :: noseg
      integer                 :: nosys
      integer                 :: nodump
      real                    :: conc(notot,noseg)
      real                    :: segfun(noseg,nosfun)
      real                    :: func(nofun)
      real                    :: param(nopa,noseg)
      real                    :: cons(nocons)
      integer                 :: idt
      integer                 :: itime
      real                    :: volume(noseg)
      integer                 :: idump(nodump)
      integer                 :: nx
      integer                 :: ny
      integer                 :: lgrid(nx,ny)
      real                    :: bound(*)
      integer                 :: noloc
      real                    :: proloc(noloc)
      integer                 :: nodef
      real                    :: defaul(nodef)
      integer                 :: lunrep

      ! local

      integer, parameter      :: nrvar = 2
      character(len=20), save :: parnam(nrvar)
      integer, save           :: paripnt(nrvar)
      real, allocatable, save :: parval(:,:)
      logical, save           :: first = .true.
      integer                 :: parindx
      integer                 :: ierr_alloc
      double precision        :: time(1)
      character(len=20), save :: streamname
      character(len=20), save :: datasetname
      integer, save           :: diooutstream
      integer, save           :: diooutset
      character, allocatable, save :: locs(:)
      character*(dioMaxTimLen) tims(1)
      logical, save           :: l_waq2flow
      character(len=256)      :: inifil
      logical                 :: lfound
      integer                 :: idummy
      real                    :: rdummy
      integer                 :: ierr2
      integer                 :: lunini
      integer                 :: ioerr
      character(len=20)       :: c20
      character(len=2)        :: c2

      ! initialise
      if ( first ) then


         first = .false.
         l_waq2flow = .false.

         ! check options from ini file

         call getcom ( '-i'  , 3    , lfound, idummy, rdummy, inifil, ierr2)
         if ( lfound ) then
            if ( ierr2.ne. 0 ) then
               inifil = ' '
            endif
         else
            inifil = 'delwaq.ini'
         endif

         open ( newunit = lunini , file=inifil , status='old' , iostat = ioerr )

         if ( ioerr .eq. 0 ) then

            call gkwini ( lunini , 'SimulationOptions', 'waq2flow' , c2 )
            if ( c2 .eq. '-1' ) then

               l_waq2flow = .true.
               write(lunrep,*) ' waq2flow coupling activated'

	       ! parameter one

               call gkwini ( lunini , 'SimulationOptions', 'parameter_waq2flow01' , c20 )
               if ( c20 .ne. ' ' ) then
                  parnam(1) = c20
                  write(lunrep,*) ' using parameter_waq2flow01 :',trim(parnam(1))
               else
                  parnam(1) = 'actths1'
                  write(lunrep,*) ' parameter_waq2flow not found, using default actths1'
               endif

               ! look for parameter in normal output

               call zoek(parnam(1),nrvart,ounam,20,parindx)
               if ( parindx .gt. 0 ) then
                  paripnt(1) = ipoint(parindx)
                  if ( paripnt(1) .le. 0 ) then
                     call getmlu(lunrep)
                     write(*,*)      'error waq2flow: parameter 1 not available'
                     write(lunrep,*) 'error waq2flow: parameter 1 not available'
                     call srstop(1)
                  endif
               else
                  call getmlu(lunrep)
                  write(*,*)      'error waq2flow: parameter not available'
                  write(lunrep,*) 'error waq2flow: parameter not available'
                  call srstop(1)
               endif

	       ! parameter two

               call gkwini ( lunini , 'SimulationOptions', 'parameter_waq2flow02' , c20 )
               if ( c20 .ne. ' ' ) then
                  parnam(2) = c20
                  write(lunrep,*) ' using parameter_waq2flow02 :',trim(parnam(2))
               else
                  parnam(2) = 'actths1'
                  write(lunrep,*) ' parameter_waq2flow not found, using default actths1'
               endif

               ! look for parameter in normal output

               call zoek(parnam(2),nrvart,ounam,20,parindx)
               if ( parindx .gt. 0 ) then
                  paripnt(2) = ipoint(parindx)
                  if ( paripnt(2) .le. 0 ) then
                     call getmlu(lunrep)
                     write(*,*)      'error waq2flow: parameter 2 not available'
                     write(lunrep,*) 'error waq2flow: parameter 2 not available'
                     call srstop(1)
                  endif
               else
                  call getmlu(lunrep)
                  write(*,*)      'error waq2flow: parameter not available'
                  write(lunrep,*) 'error waq2flow: parameter not available'
                  call srstop(1)
               endif

               ! allocate output array

               allocate(parval(nrvar,noseg),locs(noseg),stat=ierr_alloc)
               if ( ierr_alloc .ne. 0 ) then
                  write(*,*)      'error waq2flow: allocating work array'
                  write(lunrep,*) 'error waq2flow: allocating work array'
                  call srstop(1)
               endif

               ! create DelftIO stream

               streamname   = 'waq2flow'
               datasetname  = 'datawaq2flow'

               diooutstream = diocreatestreamsynched(dio_binary_stream, streamname, 'w')
!jvb           diooutset    = diofpltdefine_11(datasetname, dio_plt_real, nrvar, parnam, noseg)
               diooutset    = diodefinepltdataset(diooutstream,datasetname, dio_plt_real, nrvar, parnam, noseg, locs)
               write(*,*)  'success in waq2flow!'
            endif
            close ( lunini )
         endif

      endif

      if ( l_waq2flow ) then

         ! fill output array

         call fioutv ( parval, paripnt , nrvar , nocons, nopa  ,
     +                 nofun , nosfun  , notot , conc  , segfun,
     +                 func  , param   , cons  , idt   , itime ,
     +                 volume, noseg   , nosys , nodump, idump ,
     +                 nx    , ny      , lgrid , 1     , bound ,
     +                 noloc , proloc  , nodef , defaul)

         ! communicate

         time(1) = itime*86400.d0
         tims(1) = ' '
         call dioputpltdatasetreals(diooutset, tims(1), nrvar, noseg, parval)

      endif

      return
      end

      end module m_waq2flow
