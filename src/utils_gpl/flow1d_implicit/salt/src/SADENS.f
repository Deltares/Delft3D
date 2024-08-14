      subroutine sadens (nbran ,ngrid ,juer  ,branch ,tw   ,csa2   ,
     &                   rho   ,ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SADENS (SAlt DENSity)
c
c Module description: Calculate water density, given the salt concentra-
c                     tion.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  6 csa2(ngrid)       I  Salt concentration in every grid point at time
c                         t(n+1).
c  3 juer              P  -
c  8 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c  7 rho(ngrid)        O  Density of diluted water per grid point.
c  5 tw(nbran)         I  Temperature of water in branch.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sadens.pf,v $
c Revision 1.3  1995/05/30  09:56:05  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:05:56  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:37  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:33:27  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nbran   ,ngrid ,juer  ,ker
      integer    branch(4,nbran)
      real       tw    (nbran)  ,csa2(ngrid) ,rho(ngrid)
c
c     Declaration of local parameters
c
      integer ibr ,igr ,i1  ,i2
      real    z1  ,z2  ,z3  ,t1  ,t2  ,t3  ,t4  ,tw2  ,dis
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
      do 20 ibr = 1,nbran
c
c        Calculation of quantities that are constant in a branch.
c        [ Doc. S-FO-001.5KV / Eq. 19-2 ]
c
         tw2 = tw(ibr)**2
         z1  = 5890.  + 38.   *tw(ibr) - .375  *tw2
         z2  = 1779.5 + 11.25 *tw(ibr) - .0745 *tw2
         z3  = 3.8    + .01   *tw(ibr)
c
c        [ Doc. S-FO-001.5KV / Eq. 19-5 ]
         t1  = 1000.*z1
         t2  = (z3-2.094) * 1000.
         t3  = (8376.*z1 + 12000.*z2) * 1000.
         t4  = 1. / (1.396*z1 + 2.*z2)
c
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
         do 10 igr = i1,i2
c
c           Calculate density in a grid point.
c           [ Doc. S-FO-001.5KV / Eq. 19-5 ]
c
            dis = (t2*csa2(igr)+t1)**2 + t3*csa2(igr)
            if (dis .lt. 0. ) then
               ker = fatal
               call error (juer,'SADENS',esaden,ker)
               goto 1000
            endif
            rho(igr) = (t1 +t2*csa2(igr) + sqrt(dis)) * t4
   10    continue
   20 continue
c
 1000 continue
c
      end
