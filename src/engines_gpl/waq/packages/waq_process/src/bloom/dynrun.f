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
!  *         SUBROUTINE DYNRUN TO SOLVE BLOOM PROBLEM                  *
!  *********************************************************************
!
!  Dynamic version of subroutine RUN. Initial conditions are specified
!  by the caller. Final conditions are returned to the caller.
!
!  This module sets up the call to the actual BLOOM II modules.
!  It is based upon similar, though not identical versions used in
!  JSBACH and the DELWAQ - BLOOM II coupling.
!
!  Boundary conditions used in the computation of constraints for
!  BLOOM II are determined by computations in other program modules
!  for example for nutrients.
!  The program returns several variables such as the total biomass
!  expressed in various units, which are not returned in the stand-alone
!  version of BLOOM II.

      subroutine dynrun(exttot,extb,tmp,sol,dep,dayl,id,iseg,nset,extlim,deat,totchl,totdry,totcar,swblsa)

      use bloom_data_dim
      use bloom_data_size 
      use bloom_data_io  
      use bloom_data_phyt    
      use bloom_data_putin   
      use bloom_data_sumou   
      use bloom_data_xvect   

      implicit none

      character*8 :: cdate
      integer     :: nonun(mt)
      integer     :: i, id, iseg, k, nset, numun
      integer     :: swblsa, infeas
      real(8)     :: sol
      real(8)     :: solpar
      real(8)     :: dep
      real(8)     :: tmp
      real(8)     :: extb
      real(8)     :: dayl
      real(8)     :: deat
      real(8)     :: totchl
      real(8)     :: exttot
      real(8)     :: extlim
      real(8)     :: totdry
      real(8)     :: totcar
      
!  Calculate solarradion level for week; correct for total radiadion.
      solpar=solaco * sol

!  Calculate mixing depths of species.
      do k=1,nuspec
         dmix(k)=sdmix(k) * dep
      end do

!  Construct date indicator.
!  Print heading for output on unit outdbg if "DUMP" is specified.
      write (cdate, 115) iseg, id
115   format (i5,1x,i2)
      if ( idump .ne. 0) then
         write (outdbg,99960) iseg, id
         write (outdbg,99950) tmp, solpar, dep
      end if

!  Call subroutine BLOOM to set up and solve the linear programs
!  for week I; BLOOM will call all other subroutines
!  to solve the problem.
!  **** Update for ECOLUMN version:
!       TOTDRY (total dry weight) passed in position NUCOLS+2 of XDEF.
      call bloom(cdate,tmp,solpar,extb,dayl,deat,dep,xinit,xdef,xeco,totchl,exttot,extlim,nset,infeas,nonun,numun,swblsa)
      totdry = xdef(nucols+2)
      totcar = 0.0
      do i = 1, nuspec
         totcar = totcar + xdef(i+nurows)/ctodry(i)
      end do

99980 format(2X,'The following species have reduced relative depth',/,
     1       2X,'for buoyancy control: ')
99970 format(2X,'Species ',A8,' has relative depth of ',F5.2)
99960 format (/,23X,'******',2X,' SEGMENT ',I8,' WEEK ',
     1       I2, 2X,'******',/)
99950 format(2X,'Important parameter values for this week:',/,
     1       2X,'Temperature =',F5.1,4X,'Solar radiation =',F8.1,
     2       4X,'Total depth =',F5.2)
99930 format (' Period: ',I4,' Potential degeneracy for species: ',20I3)
      return
      end
