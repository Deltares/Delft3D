      subroutine saabcd (ngrid  ,ngridm ,nstru  ,i1     ,i2     ,dt    ,
     &                   psi    ,theta  ,q1     ,q2     ,qltgim ,csa1  ,
     &                   csd1   ,source ,disgr  ,x      ,at1    ,at2   ,
     &                   af     ,strtyp ,salstr ,strclo ,strhis ,
     &                   aa     ,ba     ,da     ,ea     ,fd     ,gd    ,
     &                   md     ,nd     ,ra     ,rd     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAABCD (SAlt A,B,(C),D,e-etc. coeff. calculation)
c
c Module description: Calculate the A,B,D,E-etc. coefficients for each
c                     gridpoint in a branch.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 23 aa                P  -
c 19 af                P  -
c 17 at1               P  -
c 18 at2               P  -
c 24 ba                P  -
c 12 csa1              P  -
c 13 csd1              P  -
c 25 da                P  -
c 15 disgr             P  -
c  6 dt                P  -
c 26 ea                P  -
c 27 fd(ngridm)        O  f-coefficient [cs(i)] in diffusion equation
c                         for every grid point in a branch.
c 28 gd                P  -
c  4 i1                I  Index of first grid point in actual branch.
c  5 i2                I  Index of last grid point in actual branch.
c 29 md                P  -
c 30 nd                P  -
c  1 ngrid             I  Number of grid points in network.
c  2 ngridm            I  Maximum number of gridpoints in a branch.
c  3 nstru             I  Number of structures.
c  7 psi               P  -
c  9 q1                P  -
c 10 q2                P  -
c 11 qltgim            P  -
c 31 ra                P  -
c 32 rd                P  -
c 21 salstr            P  -
c 14 source            P  -
c 22 strclo            P  -
c 20 strtyp(10,nstru)  I  Structure definitions:
c                         (1,i) = Type of structure:
c                                 csweir (1) : Simple weir
c                                 caweir (2) : Advanced weir
c                                 csgate (3) : - not used, reserved for
c                                                simple gate -
c                                 cpump  (4) : Pump
c                                 cgenst (5) : General Structure
c                         (2,i) = Position of structure:
c                                 cstbra (1) : In branch
c                                 cstlat (2) : Lateral structure
c                         (3,i) = Left gridpoint.
c                         (4,i) = Right gridpoint.
c                         (5,i) = dummy
c                         (6,i) = dummy
c                         (7,i) = dummy
c                         (8,i) = dummy
c                         (9,i) = dummy
c                         (10,i)= dummy
c  8 theta             P  -
c 16 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c sanorm  SAlt NORMal a,b,d,e-etc. coeff. calc.
c sastru  SAlt a,b,d,e-etc. coef. cal. STRUctures
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: saabcd.pf,v $
c Revision 1.7  1999/03/15  15:53:15  kuipe_j
c tabs removed
c
c Revision 1.6  1997/11/26  14:44:45  kuipe_j
c diffusion zero for free flow
c
c Revision 1.5  1995/10/18  09:00:11  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/08/23  14:29:35  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:56:01  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:05:50  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:31  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:16:59  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:18  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:11  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer    ngrid  ,ngridm ,nstru  ,i1     ,i2
      real       psi    ,theta
      integer    strtyp(10,*)
      real       qltgim(ngrid)  ,
     &           csa1  (ngrid)  ,csd1  (ngrid)  ,source(ngrid)  ,
     &           disgr (ngrid)  ,x     (ngrid)  ,at1   (ngrid)  ,
     &           at2   (ngrid)  ,af    (ngrid)  ,
     &           salstr(7,nstru),strhis(dmstrh,nstru)
      double precision  dt
      double precision
     &           q1    (ngrid ) ,q2   (ngrid ) ,
     &           aa    (ngridm) ,ba   (ngridm) ,da    (ngridm) ,
     &           ea    (ngridm) ,fd   (ngridm) ,gd    (ngridm) ,
     &           md    (ngridm) ,nd   (ngridm) ,ra    (ngridm) ,
     &           rd    (ngridm)
      logical    strclo(nstru)
c
c     Declaration of local parameters
c
      integer    il    ,ir    ,j     ,istr
c
      call sanorm (ngrid  ,ngridm ,i1     ,i2     ,dt     ,psi   ,
     &             theta  ,q1     ,q2     ,qltgim ,csa1   ,csd1  ,
     &             source ,disgr  ,x      ,at1    ,at2    ,af    ,
     &             aa     ,ba     ,da     ,ea     ,fd     ,gd    ,
     &             md     ,nd     ,ra     ,rd      )
c
c     Make equations for structures that reside in this branch.
c     Initialize coefficient fd, ba and ea to detect flow type in 
c     parallel structures.
c
      do 10 istr = 1 , nstru
         if (strtyp(2,istr) .eq. cstbra  .and.
     &      strtyp(3,istr) .ge. i1 .and. strtyp(3,istr) .le. i2 ) then
            il =  strtyp(3,istr)
            j  =  il - i1 + 1
            fd(j) = 0.d0
            ba(j) = 0.d0
            ea(j) = 0.d0
         endif
   10 continue
c
      do 20 istr = 1 , nstru
         if (strtyp(2,istr) .eq. cstbra  .and.
     &      strtyp(3,istr) .ge. i1 .and. strtyp(3,istr) .le. i2 ) then
            il =  strtyp(3,istr)
            ir =  strtyp(4,istr)
            j  =  il - i1 + 1
c
            call sastru (ngrid ,ngridm ,il    ,ir     ,j      ,istr  ,
     &                   q2     ,af    ,disgr ,salstr ,strclo ,strhis,
     &                   aa     ,ba    ,da    ,ea     ,fd     ,gd    ,
     &                   md     ,nd    ,ra    ,rd     )
c
         endif
   20 continue

CJK   do 99 i=1,i2-i1
CJK   WRITE (99,*)  'ADVECT: aa,ba,da,ea,fd,gd',i+i1-1
CJK   WRITE (99,*)   aa(i)  ,ba(i)  ,da(i)  ,ea(i)   ,fd(i)  ,gd(i)
CJK   WRITE (99,*)  'DIFFUS:md,nd,ra,rd',i+i1-1
CJK   WRITE (99,*)   md(i)  ,nd(i)  ,ra(i)  ,rd(i )
CJK9  continue
      end
