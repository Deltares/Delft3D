subroutine incdis(lundia    ,sferic    ,grdang    ,timnow    ,nsrcd     , &
                & lstsc     ,lstsci    ,j         ,nmmaxj    ,kmax      , &
                & icx       ,icy       ,kfsmin    ,kfsmx0    , &
                & disint    ,dismmt    ,itdis     ,kcu       ,kcv       , &
                & kfs       ,iwrk      ,mnksrc    ,alfas     ,xcor      , &
                & ycor      ,dp        ,disch     ,voldis    , &
                & disch0    ,disch1    ,rint      ,rint0     ,rint1     , &
                & umdis     ,umdis0    ,umdis1    ,vmdis     ,vmdis0    , &
                & vmdis1    ,bubble    ,r0        ,thick     ,relthk    , &
                & dzs0      ,dps       ,s0        ,qsrcrt    ,gdp       )
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
!
!    Function: Determine increments and updates the current time
!              dependent value for discharge flows and conc.
!              (if fldis = TRUE)
! Method used: At each time step (if DISINT=TRUE) 
!              New values with linear interpolation are determined
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer               , pointer :: itfinish
    integer               , pointer :: lundis
    integer               , pointer :: ltem
    integer               , pointer :: nxbub
    real(fp)              , pointer :: tstop
    real(fp)              , pointer :: hdt
    real(fp)              , pointer :: dt
    real(fp)              , pointer :: cp
    real(fp)              , pointer :: rhow
    real(fp)              , pointer :: maxTOutlet
    real(fp), dimension(:), pointer :: capacity
    real(fp)              , pointer :: eps
    real(fp)              , pointer :: scalef
    integer               , pointer :: rtcmod
    logical               , pointer :: zmodel
    logical , dimension(:), pointer :: flbub
!
! Global variables
!
    integer                                                                   :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                                        !!  then computation proceeds in the X-
                                                                                        !!  dir. If icx=1 then computation pro-
                                                                                        !!  ceeds in the Y-dir.
    integer                                                                   :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                                   :: j      !!  Begin pointer for arrays which have
                                                                                        !!  been transformed into 1D arrays.
                                                                                        !!  Due to the shift in the 2nd (M-)
                                                                                        !!  index, J = -2*NMAX + 1
    integer                                                     , intent(in)  :: kmax
    integer                                                                   :: lstsc  !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: lstsci ! Description and declaration in esm_alloc_int.f90
    integer                                                                   :: lundia !  Description and declaration in inout.igs
    integer                                                                   :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                                   :: nsrcd  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(5, nsrcd)                                         :: itdis  !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(7, nsrcd)                                         :: mnksrc !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: iwrk
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    logical                                                     , intent(in)  :: bubble !  Description and declaration in procs.igs
    logical                                                                   :: sferic !  Description and declaration in tricom.igs
    real(fp)                                                                  :: grdang !  Description and declaration in tricom.igs
    real(fp)                                                                  :: timnow !!  Current timestep (multiples of dt)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dp     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), intent(in)  :: r0     ! Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(lstsc, nsrcd)                                     :: rint   !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(lstsc, nsrcd)                                     :: rint0  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(lstsc, nsrcd)                                     :: rint1  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrcd)                                            :: disch  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrcd)                                            :: disch0 !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrcd)                                            :: disch1 !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(2,nsrcd)                                          :: qsrcrt !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrcd)                                            :: umdis  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrcd)                                            :: umdis0 !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrcd)                                            :: umdis1 !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrcd)                                            :: vmdis  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrcd)                                            :: vmdis0 !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrcd)                                            :: vmdis1 !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(nsrcd)                                            :: voldis !  Description and declaration in esm_alloc_real.f90   
    real(fp)    , dimension(kmax)                               , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp)    , dimension(kmax)                                             :: relthk
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs0   !  Description and declaration in esm_alloc_real.f90
    real(prec)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    character(1), dimension(nsrcd)                                            :: disint !  Description and declaration in esm_alloc_char.f90
    character(1), dimension(nsrcd)                                            :: dismmt !  Description and declaration in esm_alloc_char.f90
!
!> Local variables
!
    integer,external  :: newlun
    integer           :: dfil
    integer           :: ddb
    integer           :: i
    integer           :: icxy      !< MAX value of ICX and ICY 
    integer           :: isrc      !< Index number of discharge location 
    integer           :: jsrc
    integer           :: k
    integer           :: kk
    integer           :: l
    integer           :: nm        !< N,M index for discharge location
    integer           :: nst_nobub !< Number of discharges excluding bubble points
    real(fp)          :: hCap      !< heat capacity that is going to be given to 1 m^3 cooling water
    real(fp)          :: timscl    !< Multiple factor to create minutes from read times
    real(fp)          :: tInlet
    real(fp)          :: alpha     !< linear interpolation factor
    real(fp)          :: rtdis0    !< Previous  read time for discharge time-series 
    real(fp)          :: rtdis1    !< Following read time for discharge time-series
    character(200)    :: filename
!
!! executable statements -------------------------------------------------------
!
    tstop      => gdp%gdexttim%tstop
    hdt        => gdp%gdnumeco%hdt
    dt         => gdp%gdexttim%dt
    itfinish   => gdp%gdinttim%itfinish
    lundis     => gdp%gdluntmp%lundis
    ltem       => gdp%d%ltem
    nxbub      => gdp%d%nxbub
    cp         => gdp%gdheat%cp
    rhow       => gdp%gdphysco%rhow
    maxTOutlet => gdp%gddischarge%maxTOutlet
    capacity   => gdp%gddischarge%capacity
    eps        => gdp%gdconst%eps
    scalef     => gdp%gdupddis%scalef
    zmodel     => gdp%gdprocs%zmodel
    flbub      => gdp%gdbubble%flbub
    rtcmod     => gdp%gdrtc%rtcmod
    !
    timscl    = 1.0
    ddb       = gdp%d%ddbound
    icxy      = max(icx, icy)
    nst_nobub = nsrcd - nxbub
    !
    ! Change the location in case of walking discharge
    ! Skip bubble screen points (mnksrc does not match)
    !
    do i = 1, nsrcd
       if (i > nst_nobub) cycle
       if (mnksrc(7,i) == 1) then
          !
          ! it is a walking discharge
          ! Always start searching wet point at original location
          !
          isrc = mnksrc(1,i)
          jsrc = mnksrc(2,i)
          nm = jsrc + ddb + ((isrc+ddb)-1)*icxy
          if (kfs(nm) == 0) then
             call wetdis(i         ,isrc      ,jsrc      ,dp        ,xcor      , &
                       & ycor      ,kcu       ,kcv       ,kfs       ,iwrk      , &
                       & j         ,nmmaxj    ,icx       ,icy       ,gdp       )
          endif
          mnksrc(4,i) = isrc
          mnksrc(5,i) = jsrc
       endif
       if (mnksrc(7,i) == 6) then
          !
          ! Q-type power station
          ! Not the discharge, but the capacity (= amount of heat to be added to the water (per second)) is given in the input
          !
          if (comparereal(capacity(i),eps)==1) then
             !
             ! Reset disch to the capacity values stored
             ! Subroutine incdis is the only one where disch/capacity are mixed
             !
             disch(i) = capacity(i)
          endif
       endif
    enddo
    !
    ! Set flbub (indicating whether new data is read) to FALSE
    !
    if (bubble) then
       flbub(:) = .false.
    endif
    !
    ! Loop over all discharge locations nsrcd
    !
    do isrc = 1, nsrcd
       !
       ! No bubble point: mnksrc(6,i) can be used to check whether this point is inside this partition
       ! Bubble point   : mnksrc mismatches with isrc. This is repaired in cnvbub
       !
       if (isrc<=nst_nobub .and. mnksrc(6,isrc)==-1) cycle
       select case (mnksrc(7,isrc))
       case (3,4,5,7)
          !
          ! Skip when discharge is a culvert
          ! Culvert discharges are calculated in subroutine culver
          !
          cycle
       case default
          !
          ! nothing
          !
       end select
       if (timnow >= real(itdis(2,isrc),fp)) then
          !
          ! Define discharge location
          ! Test over KCS is not necessary. Discharge location always
          ! in point with KCS = 1 (see CHKDIS)
          !
          ! WARNING: nm is wrong for bubble points (due to extending mnksrc)
          ! This does not harm because momentum is not used for bubble points
          !
          nm = (mnksrc(5, isrc) + ddb) + ((mnksrc(4, isrc) - 1) + ddb)*icxy
          !
          ! Read new time step and discharge data
          !
          call upddis(lundis    ,lundia    ,sferic    ,itdis     , &
                    & isrc      ,nm        ,grdang    ,timnow    ,dt        , &
                    & itfinish  ,timscl    ,nsrcd     ,lstsc     ,j         , &
                    & nmmaxj    ,dismmt    ,alfas     , &
                    & disch0    ,disch1    ,rint0     ,rint1     , &
                    & umdis0    ,umdis1    ,vmdis0    ,vmdis1    ,gdp       )
          if (bubble) then
             flbub(isrc) = .true.
          endif
          if (disint(isrc) /= 'Y') then
               disch(isrc) = disch0(isrc)
               umdis(isrc) = umdis0(isrc)
               vmdis(isrc) = vmdis0(isrc)
               do l = 1, lstsc
                   rint(l, isrc) = rint0(l, isrc)
               enddo
          endif
       endif
       !
       if (disint(isrc) == 'Y') then
            if (itdis(1,isrc) == itdis(2,isrc)) then
               alpha = 0.0_fp
            else
               rtdis0 = real(itdis(1,isrc),fp)
               rtdis1 = real(itdis(2,isrc),fp)
               alpha  = (timnow-rtdis0) / (rtdis1-rtdis0)
            endif
            disch(isrc) = (1.0_fp-alpha)*disch0(isrc) + alpha*disch1(isrc)
            umdis(isrc) = (1.0_fp-alpha)*umdis0(isrc) + alpha*umdis1(isrc)
            vmdis(isrc) = (1.0_fp-alpha)*vmdis0(isrc) + alpha*vmdis1(isrc)
            do l = 1, lstsc
                rint(l, isrc) = (1.0_fp-alpha)*rint0(l, isrc) + alpha*rint1(l, isrc)
            enddo
       endif
       !
       ! Overrule discharge if it has been set by RTC
       !
       if (btest(rtcmod,dataFromRTCToFLOW) .and. qsrcrt(1,isrc)>0) then
           disch(isrc) = qsrcrt(2,isrc)
       endif
       !
       if (mnksrc(7,isrc) == 6) then
          !
          ! Q-type power station:
          ! - Store the new capacity in the capacity array
          ! - Calculate the new discharge
          !
          capacity(isrc) = disch(isrc)
          !
          ! get intake location
          !
          if (disch(isrc) >= 0.0_fp ) then
             nm = (mnksrc(2, isrc) + ddb) + ((mnksrc(1, isrc) - 1) + ddb)*icxy
             kk = mnksrc(3, isrc)
          else
             nm = (mnksrc(5, isrc) + ddb) + ((mnksrc(4, isrc) - 1) + ddb)*icxy
             kk = mnksrc(6, isrc)
          endif
          !
          ! get intake temperature
          !
          tInlet = 0.0_fp
          if (zmodel) then
             if (kk == 0) then
                do k = 1, kmax
                   relthk(k) = dzs0(nm,k) / (real(dps(nm),fp)+s0(nm))
                   tInlet    = tInlet + r0(nm,k,ltem)*relthk(k)
                enddo
             else
                if (kk > kfsmx0(nm)) kk = max(kfsmx0(nm), kfsmin(nm))
                if (kk < kfsmin(nm)) kk = kfsmin(nm)
                tInlet = r0(nm, kk, ltem)
             endif
          else
             !
             ! sigma model
             !
             if (kk == 0) then
                do k = 1, kmax
                   tInlet = tInlet + r0(nm, k, ltem)*thick(k)
                enddo
             else
                tInlet = r0(nm, kk, ltem)
             endif
          endif
          !
          ! calculate discharge
          !
          hCap = rhow * cp * (min(tInlet+rint(ltem,isrc),maxTOutlet)-tInlet)
          disch(isrc) = capacity(isrc) / hCap
       endif
    enddo
    !
    ! Cancel where mnksrc(6,.)=-1 (outside this domain)
    ! This can only be done here for non-bubble points (mnksrc and disch do not match)
    ! For bubble points this is done in cnvbub, when disch matches mnksrc again
    !
    do isrc = 1, nst_nobub
       if (mnksrc(6, isrc) == -1 ) disch(isrc) = 0.0_fp
       voldis(isrc) = voldis(isrc) + disch(isrc)*hdt
    enddo
end subroutine incdis
