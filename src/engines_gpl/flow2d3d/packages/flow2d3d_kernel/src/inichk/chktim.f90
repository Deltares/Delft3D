subroutine chktim(lundia    ,nostat    ,ntruv     ,itstrt    ,itfinish  , &
                & prsmap    ,prshis    ,selmap    ,selhis    ,ipmap     , &
                & maxprt    ,itmapf    ,itmapl    ,itmapi    ,iphisf    , &
                & iphisl    ,iphisi    ,ithisf    ,ithisl    ,ithisi    , &
                & itcomf    ,itcoml    ,itcomi    ,itrsti    ,itnflf    , &
                & itnfli    ,itnfll    ,error     ,gdp       )
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
!    Function: checks various output times
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                         :: iphisf   !  Description and declaration in inttim.igs
    integer                         :: iphisi   !  Description and declaration in inttim.igs
    integer                         :: iphisl   !  Description and declaration in inttim.igs
    integer                         :: itcomf   !  Description and declaration in inttim.igs
    integer                         :: itcomi   !  Description and declaration in inttim.igs
    integer                         :: itcoml   !  Description and declaration in inttim.igs
    integer                         :: ithisf   !  Description and declaration in inttim.igs
    integer                         :: ithisi   !  Description and declaration in inttim.igs
    integer                         :: ithisl   !  Description and declaration in inttim.igs
    integer                         :: itmapf   !  Description and declaration in inttim.igs
    integer                         :: itmapi   !  Description and declaration in inttim.igs
    integer                         :: itmapl   !  Description and declaration in inttim.igs
    integer                         :: itnflf   !  Description and declaration in inttim.igs
    integer                         :: itnfli   !  Description and declaration in inttim.igs
    integer                         :: itnfll   !  Description and declaration in inttim.igs
    integer                         :: itrsti   !  Description and declaration in inttim.igs
    integer           , intent(in)  :: itfinish !  Description and declaration in inttim.igs
    integer           , intent(in)  :: itstrt   !  Description and declaration in inttim.igs
    integer                         :: lundia   !  Description and declaration in inout.igs
    integer           , intent(in)  :: maxprt
    integer           , intent(in)  :: nostat   !  Description and declaration in dimens.igs
    integer           , intent(in)  :: ntruv    !  Description and declaration in dimens.igs
    integer, dimension(maxprt)      :: ipmap    !  Description and declaration in inttim.igs
    logical           , intent(out) :: error    !!  Flag=TRUE if an error is encountered
    character(*)      , intent(in)  :: selmap   !  Description and declaration in tricom.igs
    character(19)     , intent(in)  :: prsmap   !  Description and declaration in tricom.igs
    character(23)     , intent(in)  :: prshis   !  Description and declaration in tricom.igs
    character(23)     , intent(in)  :: selhis   !  Description and declaration in tricom.igs
!
! Local variables
!
    integer       :: i
    integer       :: icomv
    integer       :: idt     ! Timestep interval related to Dt (= 1) 
    integer       :: ihisv
    integer       :: imapv
    integer       :: iplus   ! Time step value to add to I"name"F or substract from I"name"L to re- define the "name" time frame inside the simulation time frame 
    integer       :: nreset  ! Index number from which number to reset MAP print times to 0 
    character(80) :: message
!
!! executable statements -------------------------------------------------------
!
    ! initialize local parameter
    !
    idt = 1
    error = .false.
    !
    ! check HIS print times according to ITSTRT and ITFINISH
    ! NOTE: some time checks are already preformed in RDTIMO
    !
    if (iphisi<=0) then
       !
       ! IPHISI < 0 is an invalid value => re-define IPHISI = 0
       !
       if (iphisi<0) then
          call prterr(lundia    ,'U046'    ,'Invalid HIS print time interval'          )
          iphisi = 0
       endif
       !
       ! if IPHISI = 0 then time frame should be IPHISF = IPHISL = 0
       !
       if (iphisl>0 .or. iphisf>0) then
          call prterr(lundia    ,'U046'    ,'Inconsistent HIS print times'  )
       endif
    endif
    !
    ! if IPHISI > 0 and NOSTAT = 0  and NTRUV = 0
    ! => re-define IPHISI = 0
    !
    if (iphisi>0) then
       if (nostat==0 .and. ntruv==0) then
          iphisi = 0
          call prterr(lundia    ,'V056'    ,' '       )
       endif
    endif
    !
    ! If no character in PRSHIS = 'Y' re-define IPHISI = 0
    !
    if (iphisi>0) then
       if (index(prshis, 'Y')==0) then
          call prterr(lundia    ,'U046'    ,'No print quant. for HIS selected'         )
          iphisi = 0
       endif
    endif
    !
    ! if ITSTRT = ITFINISH then IPHISF = IPHISL = ITSTRT
    ! or if ITSTRT <> ITFINISH then IPHISI <= (ITFINISH - ITSTRT)
    !
    if (iphisi>0) then
       if (itstrt==itfinish) then
          iphisf = itstrt
          iphisl = itstrt
       elseif (iphisi>(itfinish - itstrt)) then
          iphisi = max(idt, itfinish - itstrt)
          call prterr(lundia    ,'V009'    ,'HIS print interval (>)'        )
       else
       endif
    endif
    !
    ! if IPHISF > ITFINISH: HIS print time frame outside simulation
    ! time frame => re-define IPHISI = 0
    !
    if (iphisi>0) then
       if (iphisf>itfinish) then
          call prterr(lundia    ,'U046'    ,'Inconsistent HIS print times'  )
          iphisi = 0
       endif
    endif
    !
    ! if IPHISL < ITSTRT: HIS print time frame outside simulation
    ! time frame => re-define IPHISI = 0
    !
    if (iphisi>0) then
       if (iphisl<itstrt) then
          call prterr(lundia    ,'U046'    ,'Inconsistent HIS print times'  )
          iphisi = 0
       endif
    endif
    !
    ! if IPHISF < ITSTRT: HIS print start time moved forward N*IPHISI
    ! to become inside simulation time frame
    !
    if (iphisi>0) then
       if (iphisf<itstrt) then
          call prterr(lundia    ,'V008'    ,'HIS print start time (<)'      )
          iplus = ((itstrt - iphisf)/iphisi)*iphisi
          if (iplus<(itstrt - iphisf)) iplus = iplus + iphisi
          iphisf = iphisf + iplus
       endif
       !
       ! if IPHISL < IPHISL: ill defined HIS print time frame
       ! => re-define IPHISI = 0
       !
       if (iphisl<iphisf) then
          call prterr(lundia    ,'U046'    ,'Inconsistent HIS print times'  )
          iphisi = 0
       endif
    endif
    !
    ! if IPHISL = IPHISL = 0 while ITSTRT <> ITFINISH: ill defined HIS
    ! print time frame => re-define IPHISI = 0
    !
    if (iphisi>0) then
       if (itstrt/=itfinish) then
          if (iphisf==0 .and. iphisl==0) then
             call prterr(lundia    ,'U046'    ,'Inconsistent HIS print times'  )
             iphisi = 0
          endif
       endif
    endif
    !
    ! no HIS print file (IPHISI = 0) then re-define HIS print time
    ! frame <-IDT,-IDT>
    !
    if (iphisi==0) then
       iphisf = -idt
       iphisl = -idt
    endif
    !
    ! check his interval according to IPHISF and IPHISL
    ! if IPHISL > IPHISF and IPHISI > IPHISL - IPHISF
    ! then reset at least IDT
    !
    if (iphisl/=iphisf) then
       if (iphisi>(iphisl - iphisf)) then
          iphisi = max(idt, iphisl - iphisf)
          call prterr(lundia    ,'V009'    ,'HIS print interval (>)'        )
       endif
       !
       ! check his interval according to IPHISF and IPHISL
       ! if IPHISL > IPHISF and IPHISL - IPHISF is not a multiple of
       ! IPHISI then reset IPHISL
       !
       ihisv = ((iphisl - iphisf)/iphisi)*iphisi
       if (ihisv/=(iphisl - iphisf)) then
          iphisl = iphisf + ihisv
          call prterr(lundia    ,'V008'    ,'HIS print stop time')
       endif
    endif
    !
    ! check MAP print times according to ITSTRT and ITFINISH
    ! NOTE: some time checks are already preformed in RDTIMO
    ! no MAP prints requested then IPMAP (1) is defined as -IDT
    !
    nreset = maxprt + 1
    if (ipmap(1)<0) nreset = 2
    !
    ! If no character in PRSMAP = 'Y' re-define IPMAP (1) = -IDT
    !
    if (ipmap(1)>=0) then
       if (index(prsmap, 'Y')==0) then
          call prterr(lundia    ,'U046'    ,'No output for MAP print selected'         )
          ipmap(1) = -idt
          nreset = 2
       endif
    endif
    !
    ! If IPMAP (1) <> -IDT and ITSTRT = ITFINISH
    ! => re-define IPMAP (1) = ITSTRT
    !
    if (ipmap(1)>=0) then
       if (itstrt==itfinish) then
          call prterr(lundia    ,'U021'    ,'MAP print time set to simulation start time'         )
          ipmap(1) = itstrt
          nreset = 2
       endif
    endif
    !
    ! If IPMAP (1,MXPRT) < ITSTRT give warning about skipping those
    ! print requests
    !
    if (ipmap(1)>=0) then
       if (ipmap(1)<itstrt) then
          call prterr(lundia    ,'U021'    ,'MAP print times < simulation start time will be skipped'        )
       endif
    endif
    !
    if (ipmap(1)>=0) then
       do i = 2, maxprt
          !
          ! If IPMAP (I) = 0, no further defined print requests
          !
          if (ipmap(i)==0) then
             nreset = i
             exit
          endif
          !
          ! If IPMAP (I) > ITFINISH give warning about skipping this and
          ! following print requests
          !
          if (ipmap(i)>itfinish) then
             call prterr(lundia    ,'U021'    ,'MAP print times > simulation stop time will be skipped'         )
             nreset = i
             exit
          endif
          !
          ! If IPMAP (I) <= IPMAP (I-1) print times not a sequential
          ! order => skipp this and following print requests
          !
          if (ipmap(i)<=ipmap(i - 1)) then
             call prterr(lundia    ,'U021'    ,'No sequentually increasing MAP print times'          )
             nreset = i
             exit
          endif
       enddo
    endif
    !
    ! Reset time frame outsite requests to 0
    !
    do i = nreset, maxprt
       ipmap(i) = 0
    enddo
    !
    ! check HIS interval according to ITSTRT and ITFINISH
    ! NOTE: some time checks are already performed in RDTIMO
    !
    if (ithisi < 0) then
       call prterr(lundia    ,'U046'    ,'Invalid HIS file time interval')
       ithisi = 0
    endif
    !
    ! check HIS interval according to NOSTAT and NTRUV
    ! if ITHISI = 0 and NOSTAT > 0 or NTRUV > 0 => warning
    !
    if (ithisi == 0) then
       if (nostat>0 .or. ntruv>0) then
          call prterr(lundia    ,'V010'    ,' '       )
       endif
    endif
    if (ithisi > 0) then
       !
       ! if ITHISI > 0 and NOSTAT = 0 and NTRUV = 0
       ! => re-define ITHISI = 0
       !
       if (nostat==0 .and. ntruv==0 .and. .not. parll) then
          !
          ! parallel: should test nostat and ntruv GLOBAL
          ithisi = 0
          !
          call prterr(lundia    ,'V011'    ,' '       )
       endif
    endif
    !
    ! If no character in SELHIS = 'Y' re-define ITHISI = 0
    !
    if (ithisi > 0) then
       if (index(selhis, 'Y')==0) then
          call prterr(lundia    ,'U046'    ,'No output for HIS selected'    )
          ithisi = 0
       endif
    endif
    !
    ! check MAP times according to ITSTRT and ITFINISH
    ! NOTE: some time checks are already preformed in RDTIMO
    !
    if (itmapi<=0) then
       !
       ! ITMAPI < 0 is an invalid value => re-define ITMAPI = 0
       !
       if (itmapi<0) then
          call prterr(lundia    ,'U046'    ,'Invalid MAP storage time interval'        )
          itmapi = 0
       endif
       !
       ! if ITMAPI = 0 then time frame should be ITMAPF = ITMAPL = 0
       !
       if (itmapl>0 .or. itmapf>0) then
          call prterr(lundia    ,'U046'    ,'Inconsistent MAP file times'   )
       endif
    endif
    !
    ! check Near field computation times
    ! NOTE: some time checks are already performed in RDTIMO
    !
    if (itnfli <= 0) then
       !
       ! ITNFLI < 0 is an invalid value => re-define ITNFLI = 0
       !
       if (itnfli < 0) then
          call prterr(lundia    ,'U046'    ,'Invalid Near Field comp time interval'        )
          itmapi = 0
       endif
       !
       ! if ITNFLI = 0 then time frame should be ITNFLF = ITNFLL = 0
       !
       if (itnfll>0 .or. itnflf>0) then
          call prterr(lundia    ,'U046'    ,'Inconsistent Near Field comp times'   )
       endif
    endif
    !
    ! If no character in SELMAP = 'Y' re-define ITMAPI = 0
    !
    if (itmapi>0) then
       if (index(selmap, 'Y')==0) then
          call prterr(lundia    ,'U046'    ,'No output for MAP selected'    )
          itmapi = 0
       endif
    endif
    !
    ! if ITSTRT = ITFINISH then ITMAPF = ITMAPL = ITSTRT
    ! or if ITSTRT <> ITFINISH then ITMAPI <= (ITFINISH - ITSTRT)
    !
    if (itmapi > 0) then
       if (itstrt == itfinish) then
          itmapf = itstrt
          itmapl = itstrt
       elseif (itmapi > (itfinish - itstrt)) then
          itmapi = max(idt, itfinish - itstrt)
          call prterr(lundia    ,'V009'    ,'< MAP file interval (>)'       )
       endif
    endif
    if (ithisi > 0) then
       if (itstrt == itfinish) then
          ithisf = itstrt
          ithisl = itstrt
       elseif (ithisi > (itfinish - itstrt)) then
          ithisi = max(idt, itfinish - itstrt)
          call prterr(lundia    ,'V009'    ,'< HIS file interval (>)'       )
       endif
    endif
    !
    ! if ITMAPF > ITFINISH: MAP file time frame outside simulation
    ! time frame => re-define ITMAPI = 0
    !
    if (itmapi > 0) then
       if (itmapf > itfinish) then
          call prterr(lundia    ,'U046'    ,'Inconsistent MAP file times' )
          itmapi = 0
       endif
    endif
    if (ithisi > 0) then
       if (ithisf > itfinish) then
          call prterr(lundia    ,'U046'    ,'Inconsistent HIS file times' )
          ithisi = 0
       endif
    endif
    !
    ! if ITMAPL < ITSTRT: MAP file time frame outside simulation
    ! time frame => re-define ITMAPI = 0
    !
    if (itmapi > 0) then
       if (itmapl < itstrt) then
          call prterr(lundia    ,'U046'    ,'Inconsistent MAP file times' )
          itmapi = 0
       endif
    endif    
    if (ithisi > 0) then
       if (ithisl < itstrt) then
          call prterr(lundia    ,'U046'    ,'Inconsistent HIS file times' )
          ithisi = 0
       endif
    endif    
    !
    ! if ITMAPF < ITSTRT: MAP file start time moved forward N*ITMAPI
    ! to become inside simulation time frame
    !
    if (itmapi > 0) then
       !
       ! if ITMAPL < ITMAPF: ill defined MAP file time frame
       ! => re-define ITMAPI = 0
       !
       if (itmapl < itmapf) then
          call prterr(lundia    ,'U046'    ,'Inconsistent MAP file times'   )
          itmapi = 0
       endif
    endif
    if (ithisi > 0) then
       if (ithisl < ithisf) then
          call prterr(lundia    ,'U046'    ,'Inconsistent HIS file times'   )
          ithisi = 0
       endif
    endif
    !
    ! if ITMAPF = ITMAPL = 0 while ITSTRT <> ITFINISH: ill defined MAP
    ! file time frame => re-define ITMAPI = 0
    !
    if (itmapi > 0) then
       if (itstrt /= itfinish) then
          if (itmapf==0 .and. itmapl==0) then
             call prterr(lundia    ,'U046'    ,'Inconsistent MAP file times'   )
             itmapi = 0
          endif
       endif
    endif
    if (ithisi > 0) then
       if (itstrt /= itfinish) then
          if (ithisf==0 .and. ithisl==0) then
             call prterr(lundia    ,'U046'    ,'Inconsistent HIS file times'   )
             ithisi = 0
          endif
       endif
    endif
    !
    ! no MAP file (ITMAPI = 0) then re-define MAP file time
    ! frame <-IDT,-IDT>
    !
    if (itmapi == 0) then
       itmapf = -idt
       itmapl = -idt
    endif
    if (ithisi == 0) then
       ithisf = -idt
       ithisl = -idt
    endif
    !
    ! check MAP interval according to ITMAPF and ITMAPL
    ! if ITMAPL > ITMAPF and ITMAPI > ITMAPL - ITMAPF
    ! then reset at least IDT
    !
    if (itmapl /= itmapf) then
       if (itmapi > itmapl-itmapf) then
          itmapi = max(idt, itmapl - itmapf)
          call prterr(lundia    ,'V009'    ,'MAP interval (>)'   )
       endif
       !
       ! check MAP interval according to ITMAPF and ITMAPL
       ! if ITMAPL > ITMAPF and ITMAPL - ITMAPF is not a multiple of
       ! ITMAPI then reset ITMAPL
       !
       imapv = ((itmapl - itmapf)/itmapi)*itmapi
       if (imapv /= (itmapl - itmapf)) then
          itmapl = itmapf + imapv
          call prterr(lundia    ,'V008'    ,'MAP stop time'      )
       endif
    endif
    if (ithisl /= ithisf) then
       if (ithisi > ithisl-ithisf) then
          ithisi = max(idt, ithisl - ithisf)
          call prterr(lundia    ,'V009'    ,'HIS interval (>)'   )
       endif
       ihisv = ((ithisl - ithisf)/ithisi)*ithisi
       if (ihisv /= (ithisl - ithisf)) then
          ithisl = ithisf + ihisv
          call prterr(lundia    ,'V008'    ,'HIS stop time'      )
       endif
    endif
    !
    ! check communication times according to ITSTRT and ITFINISH
    ! NOTE: some time checks are already preformed in RDTIMO
    !
    if (itcomi <= 0) then
       !
       ! ITCOMI < 0 is an invalid value => re-define ITCOMI = 0
       !
       if (itcomi < 0) then
          call prterr(lundia    ,'U046'    ,'Invalid comm. storage time interval'      )
          itcomi = 0
       endif
       !
       ! if ITCOMI = 0 then time frame should be ITCOMF = ITCOML = 0
       !
       if (itcoml>0 .or. itcomf>0) then
          call prterr(lundia    ,'U046'    ,'Inconsistent comm. file times' )
       endif
    endif
    !
    ! if ITSTRT = ITFINISH then ITCOMF = ITCOML = ITSTRT
    ! or if ITSTRT <> ITFINISH then ITCOMI <= (ITFINISH - ITSTRT)
    !
    if (itcomi > 0) then
       if (itstrt == itfinish) then
          itcomf = itstrt
          itcoml = itstrt
       elseif (itcomi > (itfinish - itstrt)) then
          itcomi = max(idt, itfinish - itstrt)
          call prterr(lundia    ,'V009'    ,'Comm. file interval (>)'       )
       else
       endif
    endif
    !
    ! if ITCOMF > ITFINISH: Comm. file time frame outside simulation
    ! time frame => re-define ITCOMI = 0
    !
    if (itcomi > 0) then
       if (itcomf > itfinish) then
          call prterr(lundia    ,'U046'    ,'Inconsistent comm. file times' )
          itcomi = 0
       endif
    endif
    !
    ! if ITCOML < ITSTRT: Comm. file time frame outside simulation
    ! time frame => re-define ITCOMI = 0
    !
    if (itcomi > 0) then
       if (itcoml < itstrt) then
          call prterr(lundia    ,'U046'    ,'Inconsistent comm. file times' )
          itcomi = 0
       endif
    endif
    !
    ! if ITCOMF < ITSTRT: Comm. file start time moved forward N*ITCOMI
    ! to become inside simulation time frame
    !
    if (itcomi > 0) then
       if (itcomf < itstrt) then
          call prterr(lundia, 'V064', '<')
          iplus = ((itstrt - itcomf)/itcomi)*itcomi
          if (iplus<(itstrt - itcomf)) iplus = iplus + itcomi
          itcomf = itcomf + iplus
       endif
       !
       ! if ITCOML > ITFINISH: Comm. file stop time moved back N*ITCOMI
       ! to become inside simulation time frame
       !
       if (itcoml > itfinish) then
          call prterr(lundia, 'V064', '>')
          iplus = ((itcoml - itfinish)/itcomi)*itcomi
          if (iplus<(itcoml - itfinish)) iplus = iplus + itcomi
          itcoml = itcoml - iplus
       endif
       !
       ! if ITCOML < ITCOML: ill defined Comm. file time frame
       ! => re-define ITCOMI = 0
       !
       if (itcoml < itcomf) then
          call prterr(lundia    ,'U046'    ,'Inconsistent comm. file times' )
          itcomi = 0
       endif
    endif
    !
    ! if ITCOML = ITCOML = 0 while ITSTRT <> ITFINISH: ill defined Comm.
    ! file time frame => re-define ITCOMI = 0
    !
    if (itcomi > 0) then
       if (itstrt /= itfinish) then
          if (itcomf==0 .and. itcoml==0) then
             call prterr(lundia    ,'U046'    ,'Inconsistent comm. file times' )
             itcomi = 0
          !
          ! Write warning in case of DELWAQ application
          !
          elseif (itcomf == itcoml) then
             call prterr(lundia    ,'G040'    ,' '       )
          else
          endif
       endif
    endif
    !
    ! no Comm. file (ITCOMI = 0) then re-define Comm. file time
    ! frame <-IDT,-IDT>
    !
    if (itcomi == 0) then
       itcomf = -idt
       itcoml = -idt
    endif
    !
    ! check communication interval according to ITCOMF and ITCOML
    ! if ITCOML > ITCOMF and if ITCOMI > ITCOML - ITCOMF then reset to
    ! MAX of IDT and ITCOML-ITCOMF
    !
    if (itcoml /= itcomf) then
       if (itcomi > itcoml-itcomf) then
          itcomi = max(idt, itcoml - itcomf)
          call prterr(lundia    ,'V009'    ,'Comm. file interval (>)'       )
       endif
       !
       ! check communication interval according to ITCOMF and ITCOML
       ! if ITCOML > ITCOMF and ITCOML - ITCOMF is not a multiple of
       ! ITCOMI then reset ITCOML
       !
       icomv = ((itcoml - itcomf)/itcomi)*itcomi
       if (icomv /= (itcoml-itcomf)) then
          itcoml = itcomf + icomv
          call prterr(lundia    ,'V009'    ,'Comm. stop time'    )
       endif
    endif
    !
    ! check RESTART interval according to ITSTRT and ITFINISH
    ! NOTE: some time checks are already preformed in RDTIMO
    !
    if (itrsti <= 0) then
       !
       ! ITRSTI < 0 is an invalid value => re-define ITRSTI = 0
       !
       if (itrsti < 0) then
          call prterr(lundia    ,'U046'    ,'Invalid RESTART file time interval'       )
          itrsti = 0
       endif
    endif
end subroutine chktim
