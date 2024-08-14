      subroutine FLGTAR (istru  ,strpar ,sign   ,zs     ,wstr   ,
     +                   w2     ,wsd    ,zb2    ,dg     ,ds1    ,
     +                   ds2    ,cgf    ,cgd    ,cwf    ,cwd    ,
     +                   mugf   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLGTAR (FLow get General sTructure ARguments)
c
c Module description: Parameters for the general structure are extracted
c                     from the packed array strpar.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 13 cgd               O  Correction coefficient for drowned gate flow.
c 12 cgf               O  Correction coefficient for free gate flow.
c 15 cwd               O  Correction coefficient for drowned weir flow.
c 14 cwf               O  Correction coefficient for free weir flow.
c  9 dg                O  Gate opening height.
c 10 ds1               O  Delta s1 general structure.
c 11 ds2               O  Delta s2 general structure.
c  1 istru             I  Number of structure.
c 16 mugf              O  Contraction coefficient for free gate flow.
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
c  6 w2(ngrid)         IO W2 coefficient of momentum equation
c  6 w2                IO Width at right side of structure.
c  6 w2(ngrid)         IO Total width at (n+theta2) in every grid point.
c  7 wsd               O  Width structure right or left side.
c  5 wstr              O  Width at centre of structure.
c  8 zb2               IO Bed level at right side of structure.
c  4 zs                IO Bed level at centre of structure.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flgtar.pf,v $
c Revision 1.8  1999/03/15  15:49:55  kuipe_j
c tabs removed
c
c Revision 1.7  1996/04/12  13:03:48  kuipe_j
c headers, minor changes
c
c Revision 1.6  1996/01/17  14:38:25  kuipe_j
c header update
c
c Revision 1.5  1995/10/18  08:59:20  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/09/22  10:01:22  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:55:00  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:58  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:43  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:30:52  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
c Initial version
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
      double precision  sign   ,zs     ,wstr   ,w2     ,wsd    ,zb2    ,
     +                  dg     ,ds1    ,ds2    ,cgf    ,cgd    ,cwf    ,
     +                  cwd    ,mugf
c
c     Declaration of local variables:
c
      double precision  w1     ,zb1    ,wsdl   ,zbsl   ,wsdr   ,zbsr   ,
     +                  help
c
c     Fetch parameters from structure info array
c
      w1   = strpar( 1,istru)
      zb1  = strpar( 2,istru)
      wsdl = strpar( 3,istru)
      zbsl = strpar( 4,istru)
      wstr = strpar( 5,istru)
      zs   = strpar( 6,istru)
      wsdr = strpar( 7,istru)
      zbsr = strpar( 8,istru)
      w2   = strpar( 9,istru)
      zb2  = strpar(10,istru)
      dg   = strpar(11,istru)
c
c     Determine cgf, cgd, cwf, cwd, mugf
c     (flow direction dependent)
c
      if ( sign .gt. 0.0D0 ) then
         cgf  = strpar(12,istru)
         cgd  = strpar(13,istru)
         cwf  = strpar(14,istru)
         cwd  = strpar(15,istru)
         mugf = strpar(16,istru)
      else
         cgf  = strpar(17,istru)
         cgd  = strpar(18,istru)
         cwf  = strpar(19,istru)
         cwd  = strpar(20,istru)
         mugf = strpar(21,istru)
      endif
c
c     Determine flow direction dependent parameters
c
      if ( sign .gt. 0.0D0 ) then
         wsd  = wsdr
         ds1  = zs - zbsr
         ds2  = zbsr - zb2
      else
         wsd  = wsdl
         ds1  = zs - zbsl
         ds2  = zbsl - zb1
         help = w1
         w1   = w2
         w2   = help
         help = zb1
         zb1  = zb2
         zb2  = help
      endif
c
      end
