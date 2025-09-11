subroutine compdiam(frac, seddm, sedd50, sedtyp, lsedtot, &
                  & logsedsig, nseddia, logseddia, nmmax, nmlb, &
                  & nmub, xx, nxx, max_mud_sedtyp, min_dxx_sedtyp, &
                  & sedd50fld, dm, dg, dxx, dgsd)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
! Function: Determines the characteristic diameters of the sediment mixtures
!           (mud fractions excluded)
!
!!--pseudo code and references--------------------------------------------------
!
! Calculate arithmetic mean diameter by a weighted average of the diameters
! of the non-mud sediments. Divide by the total percentage of
! non-mud sediments to exclude the mud fractions from the computation.
! D_m = sum[f(i) * D_50(i)]
!
! Calculate geometric mean diameter by a weighted average of the
! diameters of the non-mud sediments in log-space.
! D_g = sum[D_50(i)^f(i)]
!
! Calculate the Dxx diameter by scanning the cdf of all
! fractions. The algorithm allows for overlapping fraction
! definitions and fractions with only one exact diameter.
! The density function of each fraction is assumed to be
! uniform in -log2(D), i.e. psi, space. Computations are
! carried out in ln(D) space.
!
!!--declarations----------------------------------------------------------------
    use precision
    use sediment_basics_module
    !
    implicit none
!
! Arguments
!
    integer                                             , intent(in)  :: lsedtot        ! number of sediment fractions
    integer                                             , intent(in)  :: nmmax          ! last space index to be processed
    integer                                             , intent(in)  :: nmlb           ! start space index
    integer                                             , intent(in)  :: nmub           ! end space index
    integer                                             , intent(in)  :: nxx            ! number of diameters to be determined
    integer                                             , intent(in)  :: max_mud_sedtyp ! largest sediment type associated with mud
    integer                                             , intent(in)  :: min_dxx_sedtyp ! smallest sediment type included in characteristic diameter computation
    integer , dimension(lsedtot)                        , intent(in)  :: nseddia        ! number of sediment diameters per fraction
    integer , dimension(lsedtot)                        , intent(in)  :: sedtyp         ! sediment type: 0=total/1=noncoh/2=coh
    real(fp), dimension(nmlb:nmub, lsedtot)             , intent(in)  :: frac           ! fractional composition of sediment
    real(fp), dimension(lsedtot)                        , intent(in)  :: seddm          ! mean diameter of sediment fraction
    real(fp), dimension(lsedtot)                        , intent(in)  :: sedd50         ! D50 of sediment fraction
    real(fp), dimension(lsedtot)                        , intent(in)  :: logsedsig      ! std deviation of sediment diameter
    real(fp), dimension(nmlb:nmub)                      , intent(in)  :: sedd50fld      ! D50 field (in case of 1 sediment fraction)
    real(fp), dimension(nxx)                            , intent(in)  :: xx             ! percentile: the xx of Dxx, i.e. 0.5 for D50
    real(fp), dimension(2,101,lsedtot)                  , intent(in)  :: logseddia      ! percentile and log-diameter per fraction
    real(fp), dimension(nmlb:nmub)                      , intent(out) :: dg             ! geometric mean diameter field
    real(fp), dimension(nmlb:nmub)                      , intent(out) :: dm             ! arithmetic mean diameter field
    real(fp), dimension(nmlb:nmub, nxx)                 , intent(out) :: dxx            ! diameters corresponding to percentiles
    real(fp), dimension(nmlb:nmub)                      , intent(out) :: dgsd           ! geometric standard deviation
!
! Local variables
!
    integer                     :: i
    integer                     :: l
    integer                     :: ltrigger
    integer                     :: n
    integer                     :: nm
    integer                     :: nn
    integer                     :: s
    integer, dimension(lsedtot) :: stage
    real(fp)                    :: dens
    real(fp)                    :: logdiam
    real(fp)                    :: logdprev
    real(fp)                    :: fracnonmud
    real(fp)                    :: fraccum
    real(fp)                    :: fracfac
    real(fp)                    :: fracreq
    real(fp)                    :: fracinterval
    real(fp)                    :: mf1
    real(fp)                    :: mf2
    real(fp)                    :: mulfac
    real(fp)                    :: p1
    real(fp)                    :: p2
    real(fp)                    :: sedsg
    real(fp)                    :: xxperc
!
!! executable statements -------------------------------------------------------
!
    if (lsedtot==1 .and. seddm(1)<0.0_fp) then
       !
       ! Handle case with spatially varying sediment diameter
       ! separately using the same approximation of the lognormal
       ! distribution used in the more general case with multiple
       ! fractions each with a constant sediment diameter.
       !
       mulfac = exp(0.5_fp * logsedsig(1) * logsedsig(1))
       do nm = 1,nmmax
          dm(nm)   = sedd50fld(nm)*mulfac
          dg(nm)   = sedd50fld(nm)
          dgsd(nm) = 0.0_fp
       enddo
       !
       sedsg = exp(logsedsig(1))
       nn    = size(ilognormal)
       do i = 1, nxx
          xxperc = xx(i) * 100.0_fp
          if (xxperc <= real(ilognormal(1),fp)) then
             p1  =  0.0_fp
             mf1 = -3.0_fp
             p2  = real(ilognormal(1),fp)
             mf2 = lognormal(ilognormal(1))
          elseif (xxperc >= real(ilognormal(nn),fp)) then
             p1  = real(ilognormal(nn),fp)
             mf1 = lognormal(ilognormal(nn))
             p2  = 100.0_fp
             mf2 =   3.0_fp
          else
             do n = 2, nn
                if (xxperc <= real(ilognormal(n),fp)) then
                   p1  = real(ilognormal(n-1),fp)
                   mf1 = lognormal(ilognormal(n-1))
                   p2  = real(ilognormal(n),fp)
                   mf2 = lognormal(ilognormal(n))
                   exit
                endif
             enddo
          endif
          mulfac = sedsg**(((p2-xxperc)*mf1 + (xxperc-p1)*mf2)/(p2-p1))
          do nm = 1,nmmax
             dxx(nm,i) = sedd50fld(nm)*mulfac
          enddo
       enddo
    else
       do nm = 1,nmmax
          !
          ! Compute Dm and Dg values
          !
          fracnonmud = 0.0_fp
          dm(nm)     = 0.0_fp
          dg(nm)     = 1.0_fp
          dgsd(nm)   = 0.0_fp
          !
          do l = 1, lsedtot
             if (sedtyp(l) >= min_dxx_sedtyp) then
                fracnonmud = fracnonmud + frac(nm,l)
             endif
          enddo
          if (fracnonmud > 0.0_fp) then
             do l = 1, lsedtot
                if (sedtyp(l) >= min_dxx_sedtyp) then
                   dm(nm)     = dm(nm) + (frac(nm,l) / fracnonmud) * seddm(l)
                   dg(nm)     = dg(nm) * (sedd50(l)**(frac(nm,l)/fracnonmud))
                endif
             enddo
             !
             ! Compute dgsd (geometric standard deviation of grain size distribution)
             ! note that this is true geometric standard deviation of grain size as there is an error in
             ! Gaeuman et al 2009. They use the arithmetic standard deviation of GSD on phi scale.
             !
             ! separate loop required as dg needs to be calculated first
             !
             do l = 1, lsedtot
                if ((sedtyp(l) >= min_dxx_sedtyp) .and. (comparereal(frac(nm,l),0.0_fp) == 1)) then
                   dgsd(nm) = dgsd(nm) + (frac(nm,l)/fracnonmud)*(log(sedd50(l))-log(dg(nm)))**2
                endif
             enddo
             dgsd(nm) = exp(sqrt(dgsd(nm)))
          else
             dg(nm) = 0.0_fp
          endif
          !
          ! Compute Dxx values
          !
          ! Initialisation: set stage and compute non-mud fraction.
          ! stage = 0 means logdiam<logseddia(2,1,l)
          !         i means logseddia(2,i,l)<logdiam<logseddia(2,i+1,l)
          !         n means logseddia(2,n,l)<logdiam where n=nseddia(l)
          ! where logdiam is used to scan through the full range of
          ! diameters. nseddia(l)=0 for mud fractions, so they are
          ! excluded from the analysis.
          !
          stage = 0
          !
          ! Fraccum indicates the cumulative fraction starting from
          ! the finest sediment up to the last diameter (logdprev).
          !
          logdprev = 0.0_fp
          fraccum  = 0.0_fp
          i        = 1
          fracreq  = xx(1)
          !
          outerfracloop: do while (fracreq > fraccum)
             !
             ! Find the smallest diameter not yet considered and calculate
             ! the density (cdf) in the considered range.
             !
             logdiam  = 999.999_fp
             ltrigger = -1
             dens     = 0.0_fp
             do l = 1, lsedtot
                s = stage(l)
                if (s<nseddia(l)) then
                   if (s>0) then
                      if (fracnonmud > 0.0_fp) then
                         fracfac = frac(nm,l) / fracnonmud
                      else
                         fracfac = 1.0_fp
                      endif
                      dens = dens + fracfac &
                                             & * (logseddia(1,s+1,l) - logseddia(1,s,l)) &
                                             & / (logseddia(2,s+1,l) - logseddia(2,s,l))
                   endif
                   if (logseddia(2,s+1,l)<logdiam) then
                      logdiam  = logseddia(2,s+1,l)
                      ltrigger = l
                   endif
                endif
             enddo
             !
             ! Check if we have not reached the end of the composition, due to
             ! some numerical roundoff errors we might not have determined the
             ! diameter of fractions equal (or close) to 100%. Finish them off
             ! now! (This may also happen if we have only mud fractions in the
             ! simulation.)
             !
             if (ltrigger < 0) then
                do while (i <= nxx)
                   dxx(nm,i) = exp(logdprev)
                   i = i+1
                enddo
                exit outerfracloop
             endif
             !
             ! Compute total fraction in the current interval.
             !
             fracinterval = dens * (logdiam-logdprev)
             do while (fracinterval >= fracreq-fraccum)
                !
                ! If the total fraction is more than what is needed,
                ! find the diameter at which we have exactly enough.
                ! We have found the requested Dxx.
                !
                logdprev     = logdprev + (logdiam-logdprev)*(fracreq-fraccum)/fracinterval
                fraccum      = fracreq
                dxx(nm,i)    = exp(logdprev)
                fracinterval = dens * (logdiam-logdprev)
                if (i < nxx) then
                   i = i+1
                   fracreq = xx(i)
                else
                   !
                   ! Diameter of last fraction determined, jump out
                   ! of loop.
                   !
                   exit outerfracloop
                endif
             enddo
             !
             ! The total fraction is less than what is needed for the
             ! currently required fraction, move to the next interval.
             !
             fraccum  = fraccum + fracinterval
             logdprev = logdiam
             !
             ! Update the stage index of the sediment fraction that
             ! is associated with the threshold currently considered.
             !
             if (stage(ltrigger) == 0) then
                !
                ! Start of the range of a new fraction. In the next
                ! range the density of this fraction should be added
                ! so, we set stage(l) to 1. However, if the range of
                ! the fraction is zero, the density function is given
                ! by a dirac function. In that case step over it and
                ! take the total fraction into account at once.
                !
                if (logseddia(2,nseddia(ltrigger),ltrigger) &
                    & <= logseddia(2,1,ltrigger)) then
                   !
                   ! The criterion may have to be extended to include
                   ! narrow ranges as well ...
                   !
                   stage(ltrigger) = 5
                   do while (frac(nm,ltrigger) > fracreq-fraccum)
                      !
                      ! If the total fraction is more than what is needed,
                      ! we have found the requested Dxx.
                      !
                      dxx(nm,i) = exp(logdiam)
                      if (i < nxx) then
                         i = i+1
                         fracreq = xx(i)
                      else
                         !
                         ! Diameter of last fraction determined, jump out
                         ! of loop.
                         !
                         exit outerfracloop
                      endif
                   enddo
                   !
                   ! If the total fraction is less than what is needed,
                   ! take the fraction into account and continue
                   ! searching for Dxx.
                   !
                   fraccum = fraccum + frac(nm,ltrigger)
                else
                   !
                   ! Range of ltrigger not too narrow, enter it.
                   !
                   stage(ltrigger) = 1
                endif
             else ! (stage(ltrigger)==1)
                !
                ! Exit range of fraction ltrigger
                !
                stage(ltrigger) = stage(ltrigger) + 1
             endif
             !
             ! Continue with next range
             !
          enddo outerfracloop
          !
          ! continue with next nm point
          !
       enddo
    endif
end subroutine compdiam
