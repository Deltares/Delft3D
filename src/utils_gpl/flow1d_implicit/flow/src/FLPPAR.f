      subroutine FLPPAR(istru  ,strpar ,ngrid  ,h1     ,il     ,ir     ,
     +                  itab   ,cap    ,capold ,hstart ,hstop  ,
     +                  iup    ,idown  ,sign   ,hact   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLPPAR (FLow get PumP ARguments)
c
c Module description: Parameters for the pump are extracted
c                     from the packed array strpar.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 cap               O  -
c 10 capold            O  -
c  4 h1(ngrid)         I  Water level in every grid point at time t(n).
c 16 hact              O  Actual water level at gridpoint in branch.
c 11 hstart            IO -
c 12 hstop             IO -
c 14 idown             O  Index of gridpoint downstream the structure.
c  5 il                I  Grid point on left side of structure (lower
c                         index).
c  6 ir                I  Grid point on right side of structure (upper
c                         index).
c  1 istru             I  Number of structure.
c  8 itab              O  Table number.
c 13 iup               O  Index of gridpoint upstream the structure.
c  3 ngrid             I  Number of grid points in network.
c 15 sign              O  Flow direction (+/-).
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
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flppar.pf,v $
c Revision 1.3  1999/03/15  15:50:28  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:04:13  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:23:47  kuipe_j
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
      integer istru, ngrid, il, ir, itab, iup, idown
      real    strpar(dmstrpar,*)
      real    cap, capold, hstart, hstop, sign, hact
      double precision h1(ngrid)
c
c     Local variables
c
      integer condir
c     
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     Determine pump parameters: condir, itab, cap, capold, hstart/stop
c     -----------------------------------------------------------------
c
      condir = int(strpar(1,istru))
      itab   = int(strpar(2,istru))
      cap    = strpar(3,istru)
      hstart = strpar(4,istru)
      hstop  = strpar(5,istru)
      capold = strpar(6,istru)
c
      if ( (hstart .lt. hstop) .eqv. (condir .eq. cpmpdw) ) then
         sign  = 1.0
         iup   = il
         idown = ir
      else
         sign  = -1.0
         iup   = ir
         idown = il
      endif
c
      if      ( condir .eq. cpmpup ) then
         hact = sngl( h1(il) )
      else if ( condir .eq. cpmpdw ) then
         hact = sngl( h1(ir) )
      endif
c
      end
