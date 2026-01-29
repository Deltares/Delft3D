subroutine FLWIFR (nbran  ,ngrid  ,branch ,time   ,&
&maxtab ,ntabm  ,ntab   ,table  ,gangle ,&
&wndpar ,wfrict ,wshld  ,tauwi  ,dt1    ,theta2)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLWIFR (FLow WInd FRiction)
!
! Module description: In subroutine FLWIFR the wind friction term tauwi
!                     will be computed.
!
!                     Given the wind parameters
!
!                     -      density of air RHO-air
!                     -      wind coefficient Cwi
!                     -      wind coefficient tau-wi
!                     -      wind speed uwi and wind direction phi-wi
!                     -      orientation of the channel phi for each
!                            grid point
!
!                     the wind friction term follows from formulaes (5-
!                     12 and 5-13) in S-FO-001.5KV. The wind direction
!                     and wind speed can be defined differently for each
!                     branch.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 14 dt1               I  Time step.
!  9 gangle(ngrid)     I  Angle of each grid point (for wind).
!  5 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
!  2 ngrid             I  Number of grid points in network.
!  7 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
!  6 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  8 table             P  -
! 13 tauwi(ngrid)      O  Calculated wind friction for each gridpoint.
! 15 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  4 time              I  Actual time level tn+1. in sec.
! 11 wfrict(3,nbran)   I  Wind friction parameters in branch.
!                         (1,i) = Indicates wind defined for branch:
!                                 cnwndf (0) : No wind defined
!                                 cywndf (1) : Wind defined
!                         (2,i) = Table pointer for wind direction as a
!                                 function of time.
!                         (3,i) = Table pointer for wind velocity as a
!                                 function of time.
! 10 wndpar(3)         I  Wind parameters for whole model:
!                         (1) = ALPHA-wi,1
!                         (2) = ALPHA-wi,2
!                         (3) = RHO-air
! 12 wshld(ngrid)      I  Wind shielding factor for each grid point.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flwifr.pf,v $
! Revision 1.5  1995/09/22  10:02:33  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:11:07  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.3  1994/11/28  08:37:58  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:50  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:57  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer maxtab, ntabm, ntab(4,maxtab),&
   &nbran, branch(4,nbran), wfrict(3,nbran),&
   &ngrid
   real    table(ntabm), gangle(ngrid), wndpar(3),&
   &wshld(ngrid), tauwi(ngrid),theta2
   double  precision     time,dt1
!
!     Declaration of local variables:
!
   integer          i, i1, i2, ibr, itab
   real             phiwbr, cwi, uwi, alpwi1, alpwi2, rhoair, phiwi
   double precision pi
   double precision timnew, timold, tnpth2
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
   pi = atan(1.D0) * 4.D0
!
   alpwi1 = wndpar(1)
   alpwi2 = wndpar(2)
   rhoair = wndpar(3)
!
!     Loop over branches
!
   do 100 ibr = 1, nbran
!
!        i1 = global grid point number at node n1 of branch
!        i2 = global grid point number at node n2 of branch
!
      i1 = branch (3,ibr)
      i2 = branch (4,ibr)
!
!       Wind defined for this branch
!
      if (wfrict(1,ibr) .eq. cywndf) then
!
!          Compute wind direction for branch at time level (n+theta2)
!
         timnew = time
         timold = time - dt1
         tnpth2 = dble (theta2*timnew + (1.-theta2)*timold)
         itab = wfrict(2,ibr)
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &tnpth2, phiwbr )
!
!          Compute wind velocity for branch at time level (n+1/2)
!
         itab = wfrict(3,ibr)
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &tnpth2, uwi )
!
         cwi = alpwi1 + alpwi2 * uwi
!
!           Loop over grid points in branch
!
         do 10 i = i1, i2
            phiwi = phiwbr - gangle(i)
            tauwi(i) = rhoair * cwi * wshld(i) * uwi * uwi *&
            &real(cos(phiwi*pi/180.D0), kind=kind(tauwi))
10       continue
!
!          No wind defined for this branch
!
      else
         do 20 i = i1, i2
            tauwi(i) = 0.
20       continue
      endif

100 continue
!
end
