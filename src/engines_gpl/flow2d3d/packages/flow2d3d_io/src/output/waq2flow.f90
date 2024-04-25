subroutine waq2flow(dps, mmax, nmaxus, kmax, lundia, mlb, mub, nlb, nub, gdp)
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
!!--description-----------------------------------------------------------------
!
!    Function: - Receive updated bed level
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dio_plt_rw
    use globaldata
    !
    implicit none
    type(globdat),target :: gdp

!
! Global variables
!
    integer                                , intent(in)  :: mlb
    integer                                , intent(in)  :: mub
    integer                                , intent(in)  :: nlb
    integer                                , intent(in)  :: nub
    integer                                , intent(in)  :: mmax
    integer                                , intent(in)  :: nmaxus
    integer                                , intent(in)  :: kmax
    integer                                , intent(in)  :: lundia
    real(prec), dimension(nlb:nub, mlb:mub)              :: dps    !  Description and declaration in esm_alloc_real.f90

    real(fp), dimension(:,:)   , pointer :: vegh2d
    real(fp), dimension(:,:)   , pointer :: vden2d
    type (gd_trachy)           , pointer :: gdtrachy
!
! Local variables
!
    integer, external                 :: diocreatestreamsynched
    integer, external                 :: dioGetPltDataSetInfo
    integer, save                     :: diooutset
    integer, save                     :: diooutstream
    integer                           :: ierr_alloc
    integer                           :: ilumon
    integer                           :: istep,iseg,m,n
    integer                           :: noseg
    integer                           :: nrtims
    integer                           :: nrvar
    logical, external                 :: diogetpltdatasetreals
    logical, save                     :: first = .true.
    logical                           :: success
    real, allocatable     , save      :: dps0(:,:)
    real, allocatable     , save      :: parval(:,:)
    real, allocatable     , save      :: thick(:,:) ! sediment thickness in m
    real, allocatable     , save      :: thick0(:,:) ! sediment thickness in m
    double precision                  :: time(1)
    character(len=20)     , save      :: datasetname
    character, allocatable, save      :: locs(:)
    character(len=20)     , save      :: parnam(1)
    character(len=20)     , save      :: streamname
    character*(dioMaxTimLen)          :: tims(1)
!
!! executable statements -------------------------------------------------------
!
    !
    ! initialise
    !
    gdtrachy      => gdp%gdtrachy
    vegh2d        => gdp%gdtrachy%vegh2d
    vden2d        => gdp%gdtrachy%vden2d

    noseg = nmaxus*mmax*kmax
    nrvar =   2
    if (first) then
       !
       ! allocate output array
       !
       allocate(parval(nrvar,noseg),locs(noseg),stat=ierr_alloc)
       if (nrvar == 1) then
          allocate(thick(nlb:nub, mlb:mub))
          allocate(thick0(nlb:nub, mlb:mub))
          allocate(dps0(nlb:nub, mlb:mub))
       endif
       if ( ierr_alloc .ne. 0 ) then
          write(*,*)      'ERROR waq2flow: allocating work array'
          write(lundia,*) 'ERROR waq2flow: allocating work array'
          call throwexception()
       endif
       !
       write(lundia,*) '--------------------------------------------'
       write(lundia,*) '| WAQ2FLOW communication anticipated       |'
       write(lundia,*) '| quantity expected:                       |'
       write(lundia,*) '|        sediment thickness in m           |'
       write(lundia,*) '--------------------------------------------'
       !
       ! create DelftIO stream
       !
       streamname   = 'waq2flow'
       datasetname  = 'datawaq2flow'
       write(*,*) '--------------'
       write(*,*) 'FLOW: waiting for waq2flow DIO stream to open'
       diooutstream = diocreatestreamsynched(dio_binary_stream, streamname, 'r')
       write(*,*) 'FLOW: waq2flow DIO stream is open'
       write(*,*) 'FLOW: waiting for GetPltDataSet waq2flow (datawaq2flow)'
       diooutset    = dioGetPltDataSetInfo (diooutstream, datasetname, &
                      & nrvar, parnam, noseg, locs, nrtims, tims)
       write(*,*) 'FLOW: GetPltDataSet waq2flow (datawaq2flow) returned'
       !
       istep = 0
    endif
    !
    write(*,*) 'FLOW: waiting for GetPltDataSetReals waq2flow (datawaq2flow)'
    success = diogetpltdatasetreals(diooutset, tims(1), nrvar, noseg, parval)
    write(*,*) 'FLOW: GetPltDataSetReals waq2flow (datawaq2flow) returned ', success
    write(*,*) '--------------'
    !
    if (nrvar == 1) then
       iseg=mmax*nmaxus*(kmax-1)
       do m=1,mmax
          do n=1,nmaxus
             iseg=iseg+1
             thick(n,m)=parval(1,iseg)
          enddo
       enddo
       if (first) then
          first = .false.
          thick0=thick
          dps0=dps
       else
          thick=thick-thick0
          dps=dps0-thick
       endif
    endif
    if (nrvar == 2) then
       iseg=mmax*nmaxus*(kmax-1)
       do m=1,mmax
          do n=1,nmaxus
             iseg=iseg+1
             vegh2d(n,m)=parval(1,iseg)
             vden2d(n,m)=parval(2,iseg)
          enddo
       enddo
       if (first) then
          first = .false.
       endif
    endif

end subroutine waq2flow
