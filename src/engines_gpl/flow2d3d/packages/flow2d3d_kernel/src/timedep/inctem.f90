subroutine inctem(ktemp     ,timnow    ,temint    ,gdp       )
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
!              dependent value for temperature  (if fltem = TRUE)
!              which model depends on value ktemp
! Method used: At each time step (if INTTEM=TRUE) the increment
!              values (stored in D"value") are added to update
!              "value:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use physicalconsts, only: CtoKelvin
    !
    use globaldata
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
    real(fp)               , pointer :: vapres
    real(fp)               , pointer :: vapres0
    real(fp)               , pointer :: vapres1
    integer                , pointer :: ivapop
    logical                , pointer :: rhum_file
    logical                , pointer :: tair_file
    logical                , pointer :: clou_file
!
! Global variables
!
    integer                   :: ktemp  !  Description and declaration in tricom.igs
    real(fp)                  :: timnow !!  Current timestep (multiples of dt)
    character(1), intent(in)  :: temint !  Description and declaration in tricom.igs
!
! Local variables
!
    real(fp) :: alpha  ! Interpolation factor; valid interval [0,1]
    logical  :: first  ! Flag = TRUE in case a time-dependent file is read for the 1st time 
    logical  :: inttem ! Interpolation method between consecutive temperature data: N = No interpolation. Y = Linear interpolation. 
    real(fp), parameter :: fCtoKelvin = real(CtoKelvin, fp) ! conversion offset between Celsius and Kelvin

!
!! executable statements -------------------------------------------------------
!
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
    if (rhum_file .or. tair_file .or. clou_file) return
    !
    first  = .false.
    inttem = .false.
    if (temint=='Y') inttem = .true.
    !
    ! Update heat module time dependent data for TIMNOW > IT1TEM
    ! For INTTEM = .false. (block function) define new working values
    ! before reading for new time
    !
    if (timnow > real(it1tem,fp)) then
       it0tem  = it1tem
       rhum0   = rhum1 
       tdryb0  = tdryb1 
       qsun0   = qsun1  
       qradin0 = qradin1
       tback0  = tback1 
       tair0   = tair1  
       cfclou0 = cfclou1
       vapres0 = vapres1
       !
       ! Read new time dep. input
       !
       call updtem(luntem    ,ktemp     ,timnow    ,dt        ,inttem    , &
                 & first     ,gdp       )
    endif
    !
    ! For interpolation INTTEM = .true. update data with step value
    !
    if(inttem .and. it0tem /= it1tem) then
        alpha = (timnow-it0tem)/(it1tem - it0tem)
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
end subroutine inctem
