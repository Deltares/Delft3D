subroutine inimorlyr(lundia    ,error     ,nmax      ,mmax      ,nmaxus    , &
                   & nmmax     ,lsed      ,lsedtot   ,gdp       )
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2023.
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
!
!    Function: - Initialisation underlayer bookkeeping system
!
!!--declarations----------------------------------------------------------------
    use precision
    use bedcomposition_module
    use m_rdmorlyr, only: rdinimorlyr
    use m_restart_lyrs, only: restart_trim_lyrs
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                             , pointer :: i_restart
    character(256)                      , pointer :: restid
    real(fp)         , dimension(:)     , pointer :: cdryb
!
! Global variables
!
    integer                                         , intent(in)  :: lsed
    integer                                         , intent(in)  :: lsedtot
    integer                                         , intent(in)  :: lundia     !  Description and declaration in inout.igs
    integer                                         , intent(in)  :: mmax       !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nmax       !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nmaxus     !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nmmax      !  Description and declaration in esm_alloc_int.f90
    logical                                                       :: error      !  Flag=TRUE if an error is encountered
!
! Local variables
!
    integer                                       :: icx
    integer                                       :: icy
    integer                             , pointer :: iporosity
    integer                                       :: ised
    integer                             , pointer :: iunderlyr
    integer                                       :: istat
    integer                                       :: k
    integer                             , pointer :: nlyr
    integer                                       :: nm
    integer                                       :: nm2
    integer                                       :: nmlb
    integer                                       :: nmaxddb
    logical                                       :: err
    logical                                       :: ex
    logical                                       :: rst_fluff
    logical                                       :: rst_bedcmp
    character(11)                                 :: fmttmp   ! Format file ('formatted  ')
    character(300)                                :: message
    real(fp)         , dimension(lsedtot)         :: mfrac
    real(fp)                                      :: mfracsum
    real(fp)                                      :: poros
    real(fp)                                      :: rdum    !< value used for initialization of array
    real(fp)                                      :: svf
    real(prec)       , dimension(:,:)   , pointer :: bodsed
    real(fp)         , dimension(:)     , pointer :: dpsed
    real(fp)         , dimension(:,:)   , pointer :: mfluff
    real(fp)         , dimension(:,:,:) , pointer :: msed
    real(fp)         , dimension(:,:)   , pointer :: thlyr
    real(fp)         , dimension(:,:)   , pointer :: svfrac
!
!! executable statements -------------------------------------------------------
!
    i_restart          => gdp%gdrestart%i_restart
    restid             => gdp%gdrestart%restid
    cdryb              => gdp%gdsedpar%cdryb
    !
    istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'iunderlyr', iunderlyr)
    if (istat==0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'nlyr'   , nlyr)
    if (istat==0) istat = bedcomp_getpointer_realprec(gdp%gdmorlyr, 'bodsed', bodsed)
    if (istat==0) istat = bedcomp_getpointer_realfp(gdp%gdmorlyr, 'dpsed', dpsed)
    if (istat==0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr, 'iporosity', iporosity)
    if (iunderlyr==2) then
       if (istat==0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr, 'msed'     , msed)
       if (istat==0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr, 'thlyr'    , thlyr)
       if (istat==0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr, 'svfrac'   , svfrac)
    endif
    if (istat/=0) then
       call prterr(lundia, 'U021', 'Memory problem in INIMORLYR')
       call d3stop(1, gdp)
    endif
    !
    nmaxddb = gdp%d%nub - gdp%d%nlb + 1
    nmlb    = gdp%d%nmlb
    fmttmp  = 'formatted'
    !
    icx = 1
    icy = nmaxddb
    !
    istat   = 0
    rst_fluff  = .false.
    rst_bedcmp = .false.
    !
    ! Try restart
    !
    error = .false.
    if (gdp%gdrestart%rst_dp) then
       !
       ! restarting from file containing bed levels, so let's check other quantities as well
       !
       rdum = -9999999.0_fp
       if (gdp%gdmorpar%flufflyr%iflufflyr>0) then
           mfluff => gdp%gdmorpar%flufflyr%mfluff
           !
           call restart_trim_fluff ( &
                  & lundia, mfluff, rst_fluff, lsed, rdum, gdp)
       endif
       !
       call restart_trim_lyrs ( &
              & msed      ,thlyr     ,lsedtot   ,cdryb     , &
              & nlyr      ,rst_bedcmp,svfrac    ,iporosity , &
              & iunderlyr ,bodsed    ,dpsed     ,rdum      , &
              & gdp       )
    endif
    !
    ! Any parameters not obtained from restart file will be initialized using
    ! values specified in input file.
    !
    call rdinimorlyr(lsedtot, lsed, lundia, error, &
                   & gdp%griddim, gdp%gdmorlyr, gdp%gdmorpar, gdp%gdsedpar, &
                   & rst_fluff, rst_bedcmp)
    !
    ! We're done
    !
end subroutine inimorlyr
