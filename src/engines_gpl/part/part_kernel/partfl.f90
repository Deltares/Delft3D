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

module partfl_mod
    !
    !  module declarations
    !
    !  data definition module(s)
    !
    use m_stop_exit
    use m_waq_precision               ! single/double precision
    use timers
    use m_densty
    use m_diffus
    !
    !  module procedure(s)
    !
    implicit none
    !
contains
    subroutine partfl (mnmaxk, conc, volume, area, velo, &
            vdiff, const, num_constants, pblay, nosubs, &
            icvdf, num_layers, alpha, wvelo, lun2, &
            nosubc, icvdf2, lgrida)
        !
        !
        !                   Deltares
        !
        !                        d e l p a r    v3.30
        !
        !
        !     created             : july 1991, by a. markus
        !
        !
        !     revised             : july 1991, by a. markus (formulations
        !                           poole harbour)
        !                         : july 1992, by r.j. vos for 8 substances
        !                           the density difference is based on subst icvdf
        !                           **** subst icvdf must be excess temperature ****
        !
        !                         : april 1994, by a. markus:
        !                           additional convention for non-stratified
        !                           areas (see part18): dispersion = -999.0
        !                           if the density difference is smaller than
        !                           const(15)
        !
        !                         : june 1996, by rj vos
        !                           temperature has a variable index icvdf
        !
        !                         : july 1996, by rj vos
        !                           conc. array organized like delwaq
        !
        !                         : september 1996, by rj vos
        !                           do vertical exchange in normal way for rho< rho-crit
        !                           instead of a random approach (lrandm=.false.)
        !                           recalc vdiff without depth averaging at interface
        !
        !                         : 11 okt 1996: redef conc-array, error in icvdf2 corr.
        !
        !                         : 2 sept 1997: add icvdf2 as argumnet
        !
        !     function            : calculates the actual exchange between the
        !                           two layers per timestep
        !
        !     note                : the user is responsible for the contents
        !                           of this routine !
        !
        !
        !     subroutines called  : stop_exit.
        !
        !
        !     parameters          :
        !
        !     name    kind     length     funct.  description
        !     ====    ====     ======     ======  ===========
        !     alpha   real        1       input   scaling factor vert. diffusivity
        !     area    real      mnmaxk    input   surface area of grid cells
        !     conc    real  nosubs*mnmaxk input   concentration in two layers
        !     const   real      num_constants    input   user-defined constants
        !     idelt   integer     1       input   model time step
        !     icvdf   integer     1       input   subst number for vert diff.calc. top
        !     icvdf2  integer     1       input   subst number for vert diff.calc. bot
        !     lun2    integer     1       input   unit number of report file
        !     mnmaxk  integer     1       input   number of grid cells
        !     num_constants  integer     1       input   number of constants
        !     num_layers   integer     1       input   number of layers
        !     nopam   integer     1       input   actual first dimension param
        !                                         (first dimension cannot
        !                                         be zero)
        !     nosubc  integer     1       input   leading dim. conc-array
        !     nosubs  integer     1       input   number of substances
        !     param   real   nopam*mnmaxk input   user-defined parameters
        !     pblay   real        1       input   relative thickness lower layer
        !     vdiff   real      mnmaxk    in/out  vertical diffusion (in)
        !                                         diffusion/length (out)
        !     velo    real      mnmaxk    input   absolute velocity in cells
        !     volume  real      mnmaxk    input   volumes of the grid cells
        !     wvelo   real        1       input   wind velocity
        !     ----    ----     ------     ------  -----------
        !     cd      real        1       local   wind drag coefficient
        !     densb   real        1       local   density top
        !     denst   real        1       local   density bottom
        !     depth   real        1       local   total water depth
        !     difmin  real        1       local   minimal vertical diffusion
        !     diff0   real        1       local   vertical diffusion
        !     first   logical     1       local   switch
        !     gamma   real        1       local   coeff. in turbulence model [-]
        !     i       integer     1       local   help variable
        !     inocns  integer     1       local   number of constants requested
        !     lrandm  logical     1       local   random exchange when true
        !     ptlay   real        1       local   relative thickness top layer
        !     salamb  real        1       local   ambient salinity
        !     subnam  char.* 6    1       local   name of this program/file
        !     tempb   real        1       local   temperature in bottom layer
        !     tempt   real        1       local   temperature in top    layer
        !     tmpamb  real        1       local   ambient temperature
        !     tpycn   real        1       local   thickness of the pycnocline
        !     ufric   real        1       local   friction velocity
        !
        !
        !     save values between invocations
        !
        save
        !
        !     parameters
        !
        character(len = 6), parameter :: subnam = 'partfl'
        !
        !     dimensioning
        !
        real(sp), dimension(:) :: vdiff, velo, volume, area, const
        real(dp), dimension(:) :: wvelo
        real(sp), dimension(:, :) :: conc
        integer :: lgrida(*)
        !
        !     assignments
        !
        real (sp) :: cd = 1.3e-3
        real (sp) :: difmin = 5.0e-4
        logical :: first = .true.
        logical :: lrandm = .false.
        !
        !     local scalars
        !
        integer(int_wp) :: i, icvdf, icvdf2, inocns, lun2
        integer(int_wp) :: max, nosubc, ic
        integer(int_wp) :: min, mnmaxk, num_constants, num_layers, nosubs
        real   (sp) :: alpha, depth, diff0, ufric
        real   (sp) :: densb, densmn, denst, gamma
        real   (sp) :: pblay, ptlay, salamb, tempb, tempt, tex
        real   (sp) :: tmpamb, tpycn, uwstar

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("partfl", ithndl)
        !
        !     the purpose of this subroutine is to determine the vertical
        !     exchange coefficient between the two layers. this coefficient
        !     depends on the vertical diffusion coefficient and possibly
        !     on other parameters as well. its definition:
        !
        !        mass flux between layers = exchange coefficient *
        !                                   concentrion difference
        !
        !     if a diffusion coefficient is available, then this amounts
        !     to:
        !        exchange coefficient = diffusion coeff. / thickness of
        !                               intermediate layer
        !
        !
        !     note:
        !
        !     the procedure will only work for mixing processes, not for
        !     the settling of suspended particles.
        !
        !     on entry the array vdiff contains the vertical diffusion
        !     coefficient in the absence of stratification. this should be
        !     replaced by the exchange coefficient.
        !
        !     nothing should happen if segments are dry: this is best determined
        !     by the condition:  velo(..) = 0
        !
        !     in this case stratification is modelled - temperature differences
        !     between the two layers lead to density differences which lead to
        !     a reduction of the vertical mixing.
        !
        !     the vertical diffusion coefficient under non-stratified circumstances
        !     has been calculated before. so, determine the reduction due to
        !     density differences and multiply.
        !     use a relation for the coefficient and the richardson number.
        !
        !     several complications arise:
        !     1. the thickness of the pycnocline is assumed to be constant
        !        (tpycn  = 1.0 [m]) - determines effective concentration
        !        gradient
        !     2. if the segment is dry nothing happens.
        !     3. if there is no particle in the segment (conc(lay1) = 0 and
        !        conc(lay2) = 0), then nothing happens either.
        !
        !        so: in these cases the exchanged mass is set to zero.
        !
        !     4. if the density difference is smaller than a critical value,
        !        stratification is assumed absent. this is characterised in this
        !        routine by a negative diffusion coefficient. the calling
        !        routine part18 will then randomly move particles, regardless of
        !        of the mass exchange, from one layer to the other.
        !
        ! -------- initialisation of the specific parameters and constants:
        !          - the layer thickness (pblay)
        !          - the first model parameter
        !          (taken care of by the program prepstra and the
        !          subroutine rdstra)
        !          note:
        !          in this case the stratification is calculated
        !          so that there is no need for an explicitly defined
        !          stratification parameter.
        !          - specific constants are read from the input file poole.prm
        !            (by partwq)
        !            const(7): thickness of the pycnocline          [in m]
        !            const(3): salinity of ambient water          [in g/l]
        !            const(4): temperature of ambient water [in degrees c]
        !            const(8): coefficient gamma in turbulence model   [-]
        !
        !          note:
        !          this is all that should be done the first time this routine
        !          is called !
        !
        !.. only when a temperature tracer for density differences is present
        !
        if(icvdf <= 0) goto 9999
        !
        if (first) then
            first = .false.
            if(num_layers /= 2) then
                write(*, *) ' num_layers = ', num_layers
                write (*, *) ' Part presently works only for 2 layer model'
                write(lun2, *) ' Part presently works only for 2 layer model'
                call stop_exit(1)
            endif
            !
            !       either initialize pblay here,
            !       or by (indexed) constant (parameter-file)
            !
            pblay = 0.7
            !
            !       check number of constants
            !
            inocns = 12
            if (inocns  >  num_constants) then
                write(lun2, 99001) subnam, inocns, num_constants
                call stop_exit(1)
            endif
            !
            !       append local constant variables
            !
            salamb = const(3)
            tmpamb = const(4)
            tex = const(5) / const(6)
            tpycn = const(7)
            gamma = const(8)
            pblay = const(9)
            densmn = const(10)
            !
            !       icvdf2 = icvdf + 1
            if(icvdf2 <= 0.or.icvdf2 > nosubc) then
                write(*, *) 'Error:  icvdf2 out of range in partfl '
                write(*, *) 'Nosubs = ', nosubs
                write(*, *) 'Nosubc = ', nosubc
                write(*, *) 'Icvdf  = ', icvdf
                write(*, *) 'Icvdf2 = ', icvdf2
                call stop_exit(1)
            endif
            !
            ptlay = 1.0 - pblay
            !
            goto 9999
            !
            !
        endif
        !
        !.. recalculate uwstar
        !.. 1.25/1000 = density-air/density-water
        !
        !
        do i = 1, mnmaxk
            ic = lgrida(i)
            if (ic > 0) then
                uwstar = sqrt(cd * 1.25 / 1000.0 * wvelo(ic)**2)
            else
                uwstar = 0.0
            endif
            !
            !       check whether there is any flow in the segment and
            !       whether there are any particles ; if not, the segment is
            !       not interesting
            !
            if (velo(i)  /=  0.0) then
                !
                !.. be careful, conc may contain missing values.... (-999.0)
                !
                if (conc(icvdf, i)  >  0.0 .or.   &
                        conc(icvdf2, i)  >  0.0) then
                    !
                    !
                    !           actual temperature (and salinity)
                    !
                    tempt = min(tex, conc(icvdf, i)) + tmpamb
                    tempb = min(tex, conc(icvdf2, i)) + tmpamb
                    !
                    !
                    densb = densty(salamb, tempb)
                    denst = densty(salamb, tempt)
                    !
                    !.. recalculate vdiff explcitly without depth-averaging
                    !   (in the old version an estimated factor 6 was introduced for this ..)
                    !
                    ufric = sqrt(9.81 / 50.0**2) * velo(i)
                    depth = volume(i) / area(i)
                    !
                    !30/9/96    diff0 = 6.0 * vdiff (i)
                    !
                    !.. 0.58 = ( 0.5774 * 0.41 * sqrt(3.0) / 0.7
                    !   0.5774 follows from c-mu in kolmogorov-prandtl eddy viscosity
                    !   0.41 is von karman constant
                    !   0.7  is prandtl-schnmidt number
                    !   sqrt(3.0) follows from constant in turbulent kinetic energy
                    !
                    !.. note: no mixing for pblay=0.0 or pblay = 1.0 !!
                    !.. prevent this with diff0 = 5.0e-4 (difmin)
                    !
                    diff0 = 0.58 * depth * pblay * sqrt (ptlay) * &
                            sqrt (ufric * ufric * ptlay + uwstar * uwstar * pblay)
                    !
                    !.. version 3.12: introduce scaling factor for calibration
                    !
                    diff0 = diff0 * alpha
                    !
                    diff0 = max(diff0, difmin)
                    !
                    if (abs(denst - densb)  >=  densmn) then
                        !
                        vdiff(i) = diffus(densb, denst, pblay, depth, &
                                diff0, ufric, gamma)   &
                                / tpycn
                    else
                        !
                        !.. only when lrandm is true then random flip-flop in vertical
                        !.. otherwise standard vertical diffusion
                        !
                        if(lrandm) then
                            vdiff(i) = -999.0
                        else
                            vdiff(i) = diff0 / tpycn
                        endif
                    endif
                endif
            endif
            !
        end do
        !
        !     end of subroutine
        !
        9999 if (timon) call timstop (ithndl)
        return
        !
        !     formats
        !
        99001 format (' Error in ', a, &
                ', : number of constants requested :', i4, &
                ' number available :', i4, '.')
        !
    end subroutine
end module
