subroutine initem(runid, cyclic, timnow, ktemp, temint, patm, gdp)
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
!    Function: Reads the time dependent data for heat models
!              from file for the first time
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use physicalconsts, only: CtoKelvin
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: it0tem
    integer                , pointer :: it1tem
    real(fp)               , pointer :: dt
    integer                , pointer :: luntem
    real(fp)               , pointer :: cp
    real(fp)               , pointer :: fclou
    real(fp)               , pointer :: rhum
    real(fp)               , pointer :: rhum0
    real(fp)               , pointer :: rhum1
    real(fp)               , pointer :: tdryb
    real(fp)               , pointer :: tdryb0
    real(fp)               , pointer :: tdryb1
    real(fp)               , pointer :: qsun
    real(fp)               , pointer :: qsun0
    real(fp)               , pointer :: qsun1
    real(fp)               , pointer :: qradin
    real(fp)               , pointer :: qradin0
    real(fp)               , pointer :: qradin1
    real(fp)               , pointer :: tback
    real(fp)               , pointer :: tback0
    real(fp)               , pointer :: tback1
    real(fp)               , pointer :: tair
    real(fp)               , pointer :: tair0
    real(fp)               , pointer :: tair1
    real(fp)               , pointer :: cfclou
    real(fp)               , pointer :: cfclou0
    real(fp)               , pointer :: cfclou1
    real(fp)               , pointer :: gapres
    real(fp)               , pointer :: vapres
    real(fp)               , pointer :: vapres0
    real(fp)               , pointer :: vapres1
    integer                , pointer :: ivapop
    logical                , pointer :: rhum_file
    logical                , pointer :: tair_file
    logical                , pointer :: clou_file
    logical                , pointer :: fltem
!
! Global variables
!
    integer                                                    :: ktemp  !  Description and declaration in tricom.igs
    logical                                                    :: cyclic !!  Flag = TRUE if cyclic system assumed
    real(fp)                                                   :: timnow !!  Current timestep (ITSTRT * dt)
    real(fp)    , dimension(gdp%d%nmlb:gdp%d%nmub)             :: patm   !  Description and declaration in esm_alloc_real.f90
    character(*)                                               :: runid
    character(1)                                  , intent(in) :: temint !  Description and declaration in tricom.igs
!
! Local variables
!
    integer        :: itfac   ! Interpolation factor 
    integer        :: lrid    ! Length of character string runid 
    integer        :: newlun
    real(fp)       :: alpha   ! Interpolation factor; valid interval [0,1]
    logical        :: first   ! Help var. It is always set to TRUE before calling the relevant routines for the time dependent data,
                              ! because they are activated here for the first time
    logical        :: inttem  ! Interpolation method between consecutive temperature  data:
                              ! N = No interpolation. Y = Linear interpolation
    logical        :: opend   ! Help flag = TRUE when file is still open (DELFT3D) and 
    character(256) :: filnam  ! Help var. for file name 
    real(fp), parameter :: fCtoKelvin = real(CtoKelvin, fp) ! conversion offset between Celsius and Kelvin
!
!! executable statements -------------------------------------------------------
!
    fltem       => gdp%gdtmpfil%fltem
    cp          => gdp%gdheat%cp
    fclou       => gdp%gdheat%fclou
    rhum        => gdp%gdheat%rhum
    rhum0       => gdp%gdheat%rhum0
    rhum1       => gdp%gdheat%rhum1
    tdryb       => gdp%gdheat%tdryb
    tdryb0      => gdp%gdheat%tdryb0
    tdryb1      => gdp%gdheat%tdryb1
    qsun        => gdp%gdheat%qsun
    qsun0       => gdp%gdheat%qsun0
    qsun1       => gdp%gdheat%qsun1
    qradin      => gdp%gdheat%qradin
    qradin0     => gdp%gdheat%qradin0
    qradin1     => gdp%gdheat%qradin1
    tback       => gdp%gdheat%tback
    tback0      => gdp%gdheat%tback0
    tback1      => gdp%gdheat%tback1
    tair        => gdp%gdheat%tair
    tair0       => gdp%gdheat%tair0
    tair1       => gdp%gdheat%tair1
    cfclou      => gdp%gdheat%cfclou
    cfclou0     => gdp%gdheat%cfclou0
    cfclou1     => gdp%gdheat%cfclou1
    gapres      => gdp%gdheat%gapres
    vapres      => gdp%gdheat%vapres
    vapres0     => gdp%gdheat%vapres0
    vapres1     => gdp%gdheat%vapres1
    ivapop      => gdp%gdheat%ivapop
    rhum_file   => gdp%gdheat%rhum_file
    tair_file   => gdp%gdheat%tair_file
    clou_file   => gdp%gdheat%clou_file
    luntem      => gdp%gdluntmp%luntem
    dt          => gdp%gdexttim%dt
    it0tem      => gdp%gdinttim%it0tem
    it1tem      => gdp%gdinttim%it1tem
    !
    ! Initialize patm on gapres
    ! Always!
    !
    patm   = gapres
    !
    inttem = .false.
    if (temint=='Y') inttem = .true.
    !
    ! initilisation global parameters, initial pressure and common
    ! values of HEAT
    ! CP = 4180. for water, 3930. for sea water
    ! NOTE: not all will be set in UPDTEM, so they have to be defined
    !       initial
    !
    cp      = 3930.0_fp
    rhum    = 0.0_fp
    rhum0   = 0.0_fp
    rhum1   = 0.0_fp
    tdryb   = 0.0_fp
    tdryb0  = 0.0_fp
    tdryb1  = 0.0_fp
    qsun    = 0.0_fp
    qsun0   = 0.0_fp
    qsun1   = 0.0_fp
    qradin  = 0.0_fp
    qradin0 = 0.0_fp
    qradin1 = 0.0_fp
    tback   = 0.0_fp
    tback0  = 0.0_fp
    tback1  = 0.0_fp
    tair    = 0.0_fp
    tair0   = 0.0_fp
    tair1   = 0.0_fp
    cfclou  = 0.0_fp
    cfclou0 = 0.0_fp
    cfclou1 = 0.0_fp
    vapres  = 0.0_fp
    vapres0 = 0.0_fp
    vapres1 = 0.0_fp
    !
    if (rhum_file .or. tair_file .or. clou_file) return
    !
    ! define length of RUNID
    !
    call remove_leading_spaces(runid     ,lrid      )
    !
    ! Time dependent temperature
    ! Only if KTEMP > 0 FLTEM = .true. (See READMD)
    !
    if (fltem) then
       filnam = 'TMP_' // runid(:lrid) // '.tem'
       !
       ! Test if file is already opened (multi entry Delft3D)
       !
       inquire (file = filnam(:8 + lrid), opened = opend)
       if (.not.opend) then
          open (newunit=luntem, file = filnam(:8 + lrid), form = 'unformatted',         &
               & status = 'old')
       endif
       !
       ! Always rewind file, reset time parameters and read time
       ! dependent input
       !
       rewind (luntem)
       it0tem = -1
       it1tem = -1
       !
       ! Read new time dep. input
       !
       first = .true.
       call updtem(luntem    ,ktemp     ,timnow    ,dt        ,inttem    , &
                 & first     ,gdp       )
       !
       !
       ! Interpolate between IT0TEM and TIMNOW for temperature data when
       ! interpolation is requested
       !
        if(inttem .and. it0tem /= it1tem) then
            alpha = (timnow-it0tem) / (it1tem-it0tem)
        else
            alpha = 0.0_fp
        endif
        rhum   = (1.0_fp-alpha)*rhum0   + alpha*rhum1 
        tdryb  = (1.0_fp-alpha)*tdryb0  + alpha*tdryb1 
        qsun   = (1.0_fp-alpha)*qsun0   + alpha*qsun1  
        qradin = (1.0_fp-alpha)*qradin0 + alpha*qradin1
        tback  = (1.0_fp-alpha)*tback0  + alpha*tback1 
        tair   = (1.0_fp-alpha)*tair0   + alpha*tair1  
        cfclou = (1.0_fp-alpha)*cfclou0 + alpha*cfclou1
        vapres = (1.0_fp-alpha)*vapres0 + alpha*vapres1
       !
       ! For KTEMP=1 & 2 or KTEMP=4 and IVAPOP=0 the value of VAPRES
       ! will be calculated See also EASP in the routine HEATU
       !
       if (ktemp <= 2) then
          vapres = 23.38_fp * (rhum/100.0_fp) * exp(18.1_fp - 5303.3_fp/(tdryb + fCtoKelvin))
       elseif (ktemp == 4 .and. ivapop == 0) then
          vapres = 23.38_fp * (rhum/100.0_fp) * exp(18.1_fp - 5303.3_fp/(tair + fCtoKelvin))
       else
       endif
    endif
end subroutine initem
