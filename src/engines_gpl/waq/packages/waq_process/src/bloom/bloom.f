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

!  *********************************************************************
!  *    SUBROUTINE FOR SETTING UP AND SOLVING BLOOM MODEL PROBLEM      *
!  *********************************************************************

      subroutine bloom(cdate,t,csol,extb,day,death,dep,xinit,xdef,xeco,total,exttot,extlim,nset,infeas,nonun,numun,swblsa)

      use bloom_data_3dl
      use bloom_data_dim
      use bloom_data_size
      use bloom_data_arran
      use bloom_data_io
      use bloom_data_matrix
      use bloom_data_phyt
      use bloom_data_sumou
      
      implicit none

      integer irs3, i, ier, index1, index2, infeas, inow, int, inhib, ismax, iskmax, irerun
      integer j, k, l1, l2, linf, ni, nin, nset, numuni, numun, swblsa
      integer ntstot, itnum, ntsst, k1
      integer nonuni(mt),nonun(mt),irs(3),lib(mx),jkmax(ms)
      save    irs, irs3
      real(8) :: x(mx),xinit(*),xdef(*),bio(2),groot(2),oroot(2*mt),root(2),emin(mt),xeco(*),xecost(ms),zood(0:mg)
      real(8) :: t
      real(8) :: usol
      real(8) :: csol
      real(8) :: extb
      real(8) :: exttot
      real(8) :: dsol
      real(8) :: dep
      real(8) :: day
      real(8) :: daym
      real(8) :: effi
      real(8) :: extlim
      real(8) :: biomax
      real(8) :: total
      real(8) :: totst
      real(8) :: exlive
      real(8) :: ekxi
      real(8) :: zmax
      real(8) :: death
      
      character*8 cdate
      character*1 errind
      logical lsolu
      real(8), parameter :: solmin=100.0

      numuni = 0
      numun = 0

!  Calculate maximum primary production and respiration rates.
      call maxprd(t)

!  Update 900109:
!  Make a copy of the (unconverted) solar radiation level. PRINUN needs
!  this value to replicate the boundary conditions.
!  Update 28 oct 92: added DEP to argument list.
      usol = csol
      call setabc(xinit,extb,exttot,csol,dsol,t,dep,nset)

! If the light intensity is very low, declare the problem 'infeasible',
! don't rerun, only write output. But do not do this for 3DL approach the
! efficiency can be obtained in other layers with light.
      if (csol .lt. solmin .and. .not. active_3dl .and. .not. active_efft) then
         irerun = 0
         errind = ' '
         ni = 0
         go to 205
      end if

! Determine indices for interpolation of day length correction factors.
      do index2 = 1,24
         if (dl(index2) .ge. day) exit
      end do
      index1 = index2 - 1

!  Compute minimum efficiency requirements of species.
      do j=1,nuecog
         daym = daymul(index1,j) + (daymul(index2,j) - daymul(index1,j)) * (day - dl(index1)) / (dl(index2) - dl(index1))
         do k=it2(j,1),it2(j,2)
            emin(k)=(resp(k)+rmort(k)) / (pmax(k)*daym)
         end do
      end do

!  Tranform sunlight to per hour units.
      do j=1,nuspec
         surf(j) = surf(j)/day
      end do

!  Average EMIN in time and find range of extinction coefficient for
!  each species.
      do j=1, nuecog
         l1 = it2(j,1)
         l2 = it2(j,2)
         groot(2) = -1.0
         iskmax = l1
         do  k=l1,l2
            if ( .not. active_3dl ) then
               if ( .not. active_efft ) then
                  call constr(surf(k),dep,emin(k),root,j)
               else
                  effi = aveffi(k)
                  root(1) = 0.0
                  root(2) = effi*exttot/emin(k)
               endif
            else
               if ( ifix_3dl(k) .lt. 0 ) then
                  effi = effic_3dl(k,iseg_3dl)
               else
                  call effi_3dl(effi,k)
               endif
               root(1) = 0.0
               root(2) = effi*exttot/emin(k)
            endif
            aroot(2*k-1)=root(1)
            aroot(2*k)=root(2)
            if (root(2) .gt. groot(2)) then
               groot(2) = root(2)
               iskmax = k
            end if
         end do

!  New section starts here.
!  We now know the extinction roots all types of species J.
!  Replace them by the MAXIMUM, but only for types, whose KMAX is
!  positive. Do nothing if KMAX is negative.
         do k = it2(j,1),it2(j,2)
            if (k .eq. iskmax) cycle
            if (aroot(2*k) .le. 0.0) cycle
            aroot(2*k-1)=aroot(2*iskmax-1)
            aroot(2*k)=aroot(2*iskmax)
         end do

!  Calculate the maximum growth rate and the right hand side for the
!  growth constraints of each species, if option "GROCHECK" is on.
!  Copy the growth constraints to BGRO to deal with infeasible
!  solutions, which are recomputed without mortality constraints.
!  Store the type number of each group with the highest KMAX value
!  in JKMAX.
         if (lgroch .eq. 1 .or. lobfun .eq. 1) then
            call maxgro(xinit, groot, exttot, emin(iskmax), j, iskmax, dep)
            bgro(j) = b(nuexro + j)
         end if
         jkmax(j) = iskmax
      end do

! Print KMIN and KMAX roots of types if option "DUMP" is on.
      if (idump .ne. 0) then
         write (outdbg,180) (aroot(i),i=1,2*nuspec,2)
  180    format (' KMIN: ',20(f7.2,1x))
         write (outdbg,190) (aroot(i),i=2,2*nuspec,2)
  190    format (' KMAX: ',20(f7.2,1x))
      end if

!  Initialize RERUN flag to 0: do not rerun the problem.
!  Set error indicator to blank.
      irerun = 0
      errind = ' '

!  Order extinction values and determine species in intervals.
!  Initialize counter infeasible intervals and number maximum interval
!  at 0
  200 call spcsd(aroot,oroot,aco,extlim,extb,ni)
  205 nin=0
      int=0
      lsolu = .false.
      if (ni .ne. 0) go to 220
      if (idump .ne. 0) write (outdbg,210)
  210 format (5x,'No species permitted in any interval')
      infeas=1
      inhib = 0
      go to 270

!  Begin solving linear programs for each interval.
!  Set initial values.
  220 continue
      bio(1)=0.0
      bio(2)=-1.0

!----------------------------------------------------------------------
!            Solve program for biomass in each interval
!----------------------------------------------------------------------
      irs(2)=0
      inow = 0
      inhib = 0
      do 250 j=1,ni
      inow = inow + 1

!  Set "B" values for extinction coefficient rows.
      linf=0
      b(nufili)=oroot(j)
      b(nuabco)=oroot(j+1)

!  Determine allowable species for feasibility interval J.
      call exclud (j,linf,irs)
      if (linf .eq. 0) go to 230
      inow = inow - 1
      if (linf .eq. 1) go to 240

!  LINF = 2: photo inhibition.
      if (idump .ne. 0) write (outdbg,225) j
  225 format(5x,'no species in interval ',i2,' due to photo inhibition')
      nin = nin + 1
      inhib = 1
      go to 250

!  Solve, test for feasibility, and find total biomass.
  230 continue
      call solvlp(inow,x,biomax,ier,irs,nonuni,numuni,lib)
      if (ier .ne. 0 .and. inow .eq. 1) irs3 = irs(3)
      linf=ier
      if (ier .eq. 0) lsolu = .true.
  240 call print6(bio,biomax,x,xdef,inow,j,linf,irs,int,nin,nonuni,nonun,numuni,numun,lib)
  250 continue

!  Check to determine if there has been any feasible solution;
      infeas=0
      if (lsolu) go to 280
      infeas=1
      if (idump .ne. 0) write (outdbg,260)
  260 format (5x,'no solution--all intervals are infeasible')

!  Infeasible solution. Call FIXINF to deal with this problem.
  270 continue
      irs(3) = irs3
      call fixinf(xdef,bio,exttot,extb,inhib,ni,irerun,irs,infeas,errind,jkmax,aroot,cdate,swblsa)
      if (irerun .ne. 0) go to 200

!----------------------------------------------------------------------
!              START SECOND PART OF THE SUBROUTINE
!----------------------------------------------------------------------
!
!  The program will now:
!    1. Print output according to the options specified and the
!       solution obtained.
!    2. Determine grazing rate constant and re-run for this timeperiod
!       if grazing is substantial.
!    3. Print a message for non-unique solutions of there may have been
!       any.
!
!
!  Print maximum solution(s) on unit 6 if option DUMP was selected.
!  Print all summarized solutions for all zooplankton iterations
!  on unit 15.

  280 continue
      call prinsu(xdef,xeco,bio(2),total,ntstot,itnum,15)
      if (idump .ne. 0) call prinma(xdef,bio(2),total,ni,nin,int)

!  If two intervals have the same maximum biomass, print both of them
!  in the complete and summarized output.
      if (lst .eq. 1) then
        call prinsu(xst,xecost,biost,totst,ntstot,itnum,15)
        ntsst = ntstot
        if (idump .ne. 0) call prinma(xst,biost,totst,ni,nin,intst)
      end if

!  Compute the total extinction
!  Update nov 4 1992: don't divide by SDMIX. Questionable for species
!  with buoyancy regulation; definitaly incorrect for species at the
!  bottom.
      exlive = 0.0
      k1 = nurows
      do k = 1,nuspec
         k1 = k1 + 1
         if (xdef(k1) .ge. 1.d-6) then
            ekxi = ekx (k) * xdef (k1)
            exlive = exlive + ekxi
         end if
      end do
      exttot = exlive + extb

!  Print a warning message if potential non-unique solutions have been
!  determined by subroutine SOLVLP.
      if (idump. eq. 0 .or. numun .eq. 0 .or.bio(2) .le. 0.0) go to 460
      write (outdbg,450) (nonun(i),i=1,numun)
  450 format ('  The following species have minimum reduced cost = 0.0 and might replace',/
     2        '  one of the species in the bloom:',1x,20i3)
  460 continue

!  Return the converted and corrected solar radiation level as
!  CSOL in Joules / cm2 / hour.
      csol = dsol * 1.0d-4 / day

      return
      end
