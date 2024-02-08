subroutine santoss_orb(nt, as_effects, tw, uorb, unet, ang, tp, &
               & rhowat, d, hw, aw, uw, uwc, uwt, uwcrepr, uwtrepr, &
               & tc, tcu, tcd, tt, ttu, ttd, uc, ut, ucx, utx, ucy, uty, &
               & ucxrepr, utxrepr, ucrepr, utrepr, b)
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
!   The SANTOSS practical sand transport model, version 2.08  
!   Computations of the orbital motions in the nearshore morphodynamical model
!   using the formula of Abreu et al. (2010)
!
!   Extra code by Nomden (2010) to define periods and maximums of time serie (normally 7 waves)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: twopi, degrad, sqrt2
    implicit none
!
! arguments
!
    integer                 , intent(in)    :: nt            ! number of timesteps in tw and uorb; must equal 200!
    integer                 , intent(in)    :: as_effects
    real(fp), dimension(nt) , intent(in)    :: tw            ! time of the uorb
    real(fp), dimension(nt) , intent(in)    :: uorb          ! time serie of orbital velocity one wave cycle
    real(fp)                , intent(in)    :: unet          ! flow velocity
    real(fp)                , intent(in)    :: ang           ! angle between waves direction and current direction
    real(fp)                , intent(in)    :: tp            ! peak wave period
    real(fp)                , intent(in)    :: rhowat        ! local water density [kg/m3]
    real(fp)                , intent(in)    :: d             ! local water depth [m]
    real(fp)                , intent(in)    :: hw            ! local wave height
!
    real(fp)                , intent(inout) :: aw            ! characteristic horizontal displacement diameter
    real(fp)                , intent(inout) :: uw            ! characteristic wave orbital velocity amplitude
    real(fp)                , intent(inout) :: uwc           ! peak wave orbital velocity for crest
    real(fp)                , intent(inout) :: uwt           ! peak wave orbital velocity for trough
    real(fp)                , intent(inout) :: uwcrepr
    real(fp)                , intent(inout) :: uwtrepr
!
    real(fp)                , intent(out)   :: tc            ! duration of the crest (positive)
    real(fp)                , intent(out)   :: tcu           ! duration of the crest accaleration
    real(fp)                , intent(out)   :: tcd           ! duration of the crest decelleration
    real(fp)                , intent(out)   :: tt            ! duration of the trough (negative)
    real(fp)                , intent(out)   :: ttu           ! duration of the trough accaleration
    real(fp)                , intent(out)   :: ttd           ! duration of the trough decelleration
    real(fp)                , intent(out)   :: uc            ! peak crest velocity (wave+current)
    real(fp)                , intent(out)   :: ut            ! peak trough velocity (wave+current)
    real(fp)                , intent(out)   :: ucx           ! current component parralel to wave direction (crest)
    real(fp)                , intent(out)   :: utx           ! current component parralel to wave direction (trough)
    real(fp)                , intent(out)   :: ucy           ! current component perpendicular to wave direction (crest)
    real(fp)                , intent(out)   :: uty           ! current component perpendicular to wave direction (trough)
    real(fp)                , intent(out)   :: ucxrepr       ! representative crest velocity parralel to wave direction (wave+current)
    real(fp)                , intent(out)   :: utxrepr       ! representative trough velocity parralel to wave direction (wave+current)
    real(fp)                , intent(out)   :: ucrepr
    real(fp)                , intent(out)   :: utrepr
    real(fp)                , intent(out)   :: b

!
! local variables
!
      integer                             :: i
      integer                             :: i_next
      integer                             :: i_prev
      integer                             :: istat
      real(fp)                            :: deltah
      real(fp)                            :: dt
      real(fp), dimension(:), allocatable :: ab_ts
      real(fp), dimension(:), allocatable :: ub_ts
      real(fp)                            :: t_max
      real(fp)                            :: t_min
      real(fp)                            :: u_max
      real(fp)                            :: u_min
      real(fp)                            :: t_0_np
      real(fp)                            :: t_0_pn
      real(fp)                            :: ucyrepr  ! representative crest velocity perpendicular to wave direction (wave+current)
      real(fp)                            :: utyrepr  ! representative trough velocity perpendicular to wave direction (wave+current)
      real(fp)                            :: awmax
      real(fp)                            :: awmin
      real(fp)                            :: kbed
      real(fp)                            :: kbed11
      real(fp)                            :: kbed1
      real(fp)                            :: kbed2
!
!! executable statements -------------------------------------------------------
!
    allocate(ab_ts(nt), STAT = istat)
    allocate(ub_ts(nt), STAT = istat)

    dt = tp/nt

    ucx = unet*cos(ang*degrad)               ! current component parallel to wave direction (crest)
    utx = ucx                                ! current component parallel to wave direction (trough)
    ucy = unet*sin(ang*degrad)               ! current component perpendicular to wave direction (crest)
    uty = ucy                                ! current component perpendicular to wave direction (trough)

!   determine the wave periods
    do i = 1,nt
        ub_ts(i) = uorb(i) + ucx
    enddo

!   acceleration at points i
    do i = 1,nt
        i_next = i+1
        i_prev = i-1
        if (i == 1)  i_prev = nt
        if (i == nt) i_next = 1
        ab_ts(i) = (ub_ts(i_next)-ub_ts(i_prev))/(2.0_fp*dt)
    enddo

    awmax = 0.0_fp
    awmin = 0.0_fp
    do i = 1,nt
       awmax = max(awmax,ab_ts(i))
       awmin = min(awmin,ab_ts(i))
    enddo

    b=awmax/(awmax-awmin)   !this includes the effect of a counter/following current

!
!   definition of maxima and minima incl. corresponding timestep
!
    do i=1,nt
        i_prev = i-1
        if (i == 1)  i_prev = nt
        if (ab_ts(i_prev) >= 0.0_fp .and. ab_ts(i) < 0.0_fp) then
            if (i_prev /= nt) then
                t_max = tw(i)+(0.0_fp-ab_ts(i_prev))/(ab_ts(i)-ab_ts(i_prev))*(tw(i)-tw(i_prev))
            else
                t_max = tw(i)+(0.0_fp-ab_ts(i_prev))/(ab_ts(i)-ab_ts(i_prev))*(tw(i)-0.0_fp)
            endif
            u_max=ub_ts(i)
        elseif (ab_ts(i_prev) < 0.0_fp .and. ab_ts(i) >= 0.0_fp) then
            if (i_prev /= nt) then
               t_min = tw(i)+(0.0_fp-ab_ts(i_prev))/(ab_ts(i)-ab_ts(i_prev))*(tw(i)-tw(i_prev))
            else
               t_min = tw(i)+(0.0_fp-ab_ts(i_prev))/(ab_ts(i)-ab_ts(i_prev))*(tw(i)-0.0_fp)
            endif
            u_min = ub_ts(i)
         endif
    enddo

    if (u_max > 0.0_fp .and. u_min > 0.0_fp) then
        tc = tp
        tt = 0.0_fp
        if (as_effects == 0) then
            tcd = 0.5_fp*tc
            tcu = 0.5_fp*tc
            ttd = 0.5_fp*tt
            ttu = 0.5_fp*tt
        else
            tcd = t_min-t_max
            tcu = tc-tcd
            ttu = 0.0_fp
            ttd = 0.0_fp
        endif
    elseif (u_max < 0.0_fp .and. u_min < 0.0_fp) then 
        tc=0.0_fp
        tt=tp
        if (as_effects == 0) then
            tcd = 0.5_fp*tc
            tcu = 0.5_fp*tc
            ttd = 0.5_fp*tt
            ttu = 0.5_fp*tt
        else
            tcd = 0.0_fp
            tcu = 0.0_fp
            ttu = t_min-t_max
            ttd = tt-ttu
        endif
    else
        !
        !   definition of zero-crossing (neg-pos and pos-neg)
        !
        do i=1,nt
            i_prev = i-1
            if (i == 1)   i_prev = nt
            ! define zero-crossing ub_ts based on neg-pos
            if (ub_ts(i_prev) < 0.0_fp .and. ub_ts(i) >= 0.0_fp) then
                if (i_prev /= nt) then
                    t_0_np = tw(i)+(0.0_fp-ub_ts(i_prev))/(ub_ts(i)-ub_ts(i_prev))*(tw(i)-tw(i_prev))
                else
                    t_0_np = tw(i)+(0.0_fp-ub_ts(i_prev))/(ub_ts(i)-ub_ts(i_prev))*(tw(i)-0.0_fp)
                endif
            elseif (ub_ts(i_prev) >= 0.0_fp .and. ub_ts(i) < 0.0_fp) then
                if (i_prev /= nt) then
                    t_0_pn = tw(i)+(0.0_fp-ub_ts(i_prev))/(ub_ts(i)-ub_ts(i_prev))*(tw(i)-tw(i_prev))
                else
                    t_0_pn = tw(i)+(0.0_fp-ub_ts(i_prev))/(ub_ts(i)-ub_ts(i_prev))*(tw(i)-0.0_fp)
                endif
            endif
        enddo

        if (t_0_pn > t_0_np) then
            tc  = t_0_pn-t_0_np
            tt  = tp-tc
            tcu = t_max- t_0_np
        else
            tt  = t_0_np-t_0_pn
            tc  = tp-tt
            tcu = t_max+tp-t_0_np
        endif
        if (as_effects == 0) then
            tcd = 0.5_fp*tc
            tcu = 0.5_fp*tc
            ttd = 0.5_fp*tt
            ttu = 0.5_fp*tt
        else
            tcd = tc - tcu
            ttu = t_min - t_0_pn
            ttd = tt - ttu
        endif
    endif

!
!   definition of maxima and representative velocities (urms)
!   and resulting crest and trough vectors (combined with current)
!
    if (comparereal(unet,0.0_fp) == 0) then
        uwc     = u_max
        uwt     = abs(u_min)
        uw      = sqrt(0.5_fp*uwc**2 + 0.5_fp*uwt**2)
        aw      = uw*tp/twopi                ! characteristic horizontal displacement diameter
        uwcrepr = uwc*0.5_fp*sqrt2           ! representative crest velocity (wave)
        uwtrepr = uwt*0.5_fp*sqrt2           ! representative trough velocity (wave)
    else
       ! keep the values from the first call (always with unet = 0.0_fp)
    endif

    uc  =  uwc + ucx                         ! peak crest velocity (wave+current)
    ut  = -uwt + utx                         ! peak trough velocity (wave+current)

    ucxrepr =  uwcrepr + ucx                 ! representative crest velocity parallel to wave direction (wave+current)
    ucyrepr =  ucy                           ! representative crest velocity perpendicular to wave direction (wave+current)
    ucrepr  = sqrt(ucxrepr**2+ucyrepr**2)    ! representative crest velocity (wave+current)

    utxrepr = -uwtrepr + utx                 ! representative trough velocity parallel to wave direction (wave+current)
    utyrepr =  uty                           ! representative trough velocity perpendicular to wave direction (wave+current)
    utrepr  = sqrt(utxrepr**2+utyrepr**2)    ! representative trough velocity (wave+current)
    !
    deallocate(ab_ts, STAT = istat)
    deallocate(ub_ts, STAT = istat)
end subroutine santoss_orb
