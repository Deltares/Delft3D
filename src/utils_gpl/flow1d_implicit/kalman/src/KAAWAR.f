      subroutine KAAWAR(istru  ,strpar ,sign   ,h0     ,ka     ,kp     ,
     +                  n      ,p      ,wn     ,zs     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAAWAR (KAlman get Advanced Weir Arguments)
c
c Module description: Parameters for a advanced weir are extracted from
c                     array strpar.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 h0                O  -
c  1 istru             I  Number of structure.
c  5 ka                O  -
c  6 kp                O  -
c  7 n                 O  -
c  8 p                 O  -
c  3 sign              I  Flow direction (+/-).
c  2 strpar(21,nstru)  I  Each structure is characterized by a number of
c                         specific parameters. strpar (i,j) = parameter
c                         i of structure j:
c                         - Simple weir:
c                         (1,j) = Crest height Zs.
c                         (2,j) = Crest width Ws.
c                              Positive flow:
c                         (3,j) = Correction coefficient cw.
c                         (4,j) = Submergence limit Slim.
c                         (5,j) = Table pointer for drowned reduction
c                                 curve f(h2/h1).
c                              Negative flow:
c                         (6,j) = Correction coefficient cw.
c                         (7,j) = Submergence limit Slim.
c                         (8,j) = Table pointer for drowned reduction
c                                 curve f(h2/h1).
c                         - Advanced weir:
c                         (1,j) = Crest height Zs.
c                         (2,j) = Total net width Wn.
c                         (3,j) = Number of piers N.
c                              Positive flow:
c                         (4,j) = Heigth of upstream face P.
c                         (5,j) = Design head H0 of the weir.
c                         (6,j) = Pier contraction coefficient Kp.
c                         (7,j) = Abutment contraction coefficient Ka.
c                              Negative flow:
c                         (8,j) = Heigth of upstream face P.
c                         (9,j) = Design head H0 of the weir.
c                         (10,j)= Pier contraction coefficient Kp.
c                         (11,j)= Abutment contraction coefficient Ka.
c                         - Pump:
c                         (1,j) = Control direction:
c                                 cpmpup (-1) : upward control
c                                 cpmpdw (+1) : downward control
c                         (2,j) = Table pointer for pump capacitity re-
c                                 duction factor.
c                         (3,j) = Capacity.
c                         (4,j) = Water level which starts pump.
c                         (5,j) = Water level which stops pump.
c                         - General structure:
c                         (1,j) = Width left side of structure W1.
c                         (2,j) = Bed level left side of structure Zb1.
c                         (3,j) = Width structure left side Wsdl.
c                         (4,j) = Bed left side of structure Zbsl.
c                         (5,j) = Width structure centre Ws.
c                         (6,j) = Bed level centre Zs.
c                         (7,j) = Width structure right side Wsdr.
c                         (8,j) = Bed right side of structure Zbsr.
c                         (9,j) = Width right side of structure W2.
c                         (10,j)= Bed level right side of structure Zb2.
c                         (11,j)= Gate opening heigth dg.
c                              Positive flow:
c                         (12,j)= Correction coefficient for free gate
c                                 flow cgf.
c                         (13,j)= Correction coefficient for drowned
c                                 gate flow cgd.
c                         (14,j)= Correction coefficient for free weir
c                                 flow cwf.
c                         (15,j)= Correction coefficient for drowned
c                                 weir flow cwd.
c                         (16,j)= Contraction coefficient for free gate
c                                 flow MU-gf.
c                              Negative flow:
c                         (17,j)= Correction coefficient for free gate
c                                 flow cgf.
c                         (18,j)= Correction coefficient for drowned
c                                 gate flow cgd.
c                         (19,j)= Correction coefficient for free weir
c                                 flow cwf.
c                         (20,j)= Correction coefficient for drowned
c                                 weir flow cwd.
c                         (21,j)= Contraction coefficient for free gate
c                                 flow MU-gf.
c  9 wn                O  -
c 10 zs                O  -
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kaawar.pf,v $
c Revision 1.3  1999/03/15  15:51:32  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:04:36  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:12  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters:
c
      integer istru
      real    strpar(dmstrpar,*)
      real    sign, h0, ka, kp, n, p, wn, zs
c
c     Determine crest height, total net width and number of piers
c     -----------------------------------------------------------
c
      zs = strpar(1,istru)
      wn = strpar(2,istru)
      n  = strpar(3,istru)
c
c     Determine P, H0, Kp and Ka
c     (flow direction dependent)
c
      if ( sign .gt. 0.0 ) then
         p  = strpar( 4,istru)
         h0 = strpar( 5,istru)
         kp = strpar( 6,istru)
         ka = strpar( 7,istru)
      else
         p  = strpar( 8,istru)
         h0 = strpar( 9,istru)
         kp = strpar(10,istru)
         ka = strpar(11,istru)
      endif
c
      end
