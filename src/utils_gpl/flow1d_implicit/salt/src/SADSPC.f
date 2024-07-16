      subroutine sadspc (dsopt  ,nbran  ,ngrid  ,maxtab ,ntabm ,g      ,
     &                   time   ,dispf  ,branch ,ntab   ,table ,thcsum ,
     &                   grid   ,x      ,wf     ,q2     ,c     ,csa1   ,
     &                   cdcdx1 ,cdcdx2 ,distmp ,disgr  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SADSPC (SAlt DiSPersion Coefficient)
c
c Module description: Calculate a dispersion coefficient for each point
c                     in the network.
c
c                     The dispersion coefficient can be calculated in
c                     four ways. This routine calls the processing rou-
c                     tine for the selected dispersion formulation.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 branch            P  -
c 17 c                 P  -
c 19 cdcdx1            P  -
c 20 cdcdx2            P  -
c 18 csa1              P  -
c 22 disgr             P  -
c  8 dispf             P  -
c 21 distmp            P  -
c  1 dsopt             I  Option for dispersion for the whole network:
c                           cds1fu (1) : One function of place or time
c                           cds2fu (2) : Two functions of place or time
c                           cdsthh (3) : Thatcher-Harleman formulation
c                           cdsemp (4) : Empirical formulation
c  6 g                 P  -
c 13 grid              P  -
c  4 maxtab            I  Maximum number of defined tables.
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c 10 ntab              P  -
c  5 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 16 q2                P  -
c 11 table             P  -
c 12 thcsum            P  -
c  7 time              P  -
c 15 wf                P  -
c 14 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c sadslc  SAlt DiSpersion Linear Coefficient
c sadsst  SAlt DiSpersion Spatial or Time dependent
c sadsth  SAlt DiSpersion Thatcher Harl. or zwendl
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sadspc.pf,v $
c Revision 1.5  1995/10/18  09:00:19  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/08/30  12:37:16  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:56:06  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:05:59  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:41  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:08  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:35  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer   dsopt ,nbran ,ngrid ,maxtab   ,ntabm
      integer   dispf (2,3)  ,branch(4,nbran) ,ntab (4,maxtab) ,
     &          grid  (ngrid)
      real      g
      real      table (ntabm)  ,thcsum(2,nbran),x     (ngrid)  ,
     &          wf    (ngrid)  ,c     (ngrid)  ,
     &          csa1  (ngrid)  ,cdcdx1(ngrid)  ,cdcdx2(ngrid)  ,
     &          distmp(ngrid)  ,disgr (ngrid)
      double    precision       time           ,q2    (ngrid)
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      if      (dsopt .eq. cds1fu) then
c
         call sadsst (ngrid ,maxtab ,ntabm  ,time   ,dispf  ,ntab   ,
     &                table ,disgr )
c
      else if (dsopt .eq. cds2fu) then
c
         call sadslc (nbran  ,ngrid ,maxtab ,ntabm  ,time   ,dispf  ,
     &                branch ,ntab  ,table  ,distmp ,x      ,csa1   ,
     &                grid   ,disgr )
c
      else if (dsopt .eq. cdsthh .or. dsopt .eq. cdsemp) then
c
         call sadsth (nbran  ,ngrid ,maxtab ,ntabm  ,g      ,time   ,
     &                dispf  ,branch,ntab   ,table  ,thcsum ,wf     ,
     &                q2     ,c     ,cdcdx1 ,cdcdx2 ,distmp ,disgr  )
c
      endif
c
      end
