      subroutine FLAW (g      ,il     ,ir     ,ngrid  ,istru  ,strclo ,
     +                 strpar ,h      ,h1     ,q      ,q2     ,af     ,
     +                 rho    ,strhis ,asde   ,csde   ,esde   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLAW (FLow structure Advanced Weir)
c
c Module description: In subroutine FLAW the ABCDE coefficients for the
c                     stage-discharge equation are computed for an
c                     advanced weir.
c
c                     In subroutine FLQHAW the Q-H relation for the
c                     advanced weir is defined.
c
c                     The following items can be controlled:
c
c                     Wn = Width across flow section
c                     Zs = Crest level of weir
c
c                     The following input parameters are available for
c                     this type of structure:
c
c                     -   Level of crest Zs
c                     -   Total net width Wn
c                     -   Number of piers N
c                     -   Heigth of upstream face P
c                     -   Design head H0 of the weir
c                     -   Pier contraction coefficient Kp
c                     -   Abutment contraction coefficient Ka
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 11 af                P  -
c 14 asde              O  a-coefficient in stage-discharge equation for
c                         this specific structure.
c 15 csde              O  c-coefficient in stage-discharge equation for
c                         this specific structure.
c 16 esde              O  e-coefficient in stage-discharge equation for
c                         this specific structure.
c  1 g                 P  -
c  9 h1                P  -
c  8 h                 P  -
c  2 il                P  -
c  3 ir                P  -
c  5 istru             I  Number of structure.
c  4 ngrid             I  Number of grid points in network.
c 10 q                 P  -
c 12 rho               P  -
c  6 strclo            P  -
c 13 strhis(10,nstru)  O  For each structure the discharge and the
c                         parameters to be controlled must be saved to
c                         be able to write to the output file. This will
c                         be done in array strhis(8,nstru). This array
c                         will also be used to check the values of the
c                         controlled parameters or to determine if
c                         increase(open) or decrease(close) of these
c                         parameters occurs. This array will also be
c                         part of the restart file.
c                         (1,i) = Gate height
c                         (2,i) = Crest height
c                         (3,i) = Crest width
c                         (4,i) = Discharge through structure
c                         (5,i) = Gate height at previous time step
c                         (6,i) = Crest height at previous time step
c                         (7,i) = Crest width at previous time step
c                         (8,i) = Flow condition of general structure:
c                                 formno = 0, closed or other structure
c                                 formno = 1, free weir
c                                 formno = 2, drowned weir
c                                 formno = 3, free gate
c                                 formno = 4, drowned gate
c                         (9,i) = coefficient Q-H-realtion asde
c                         (10,i)= coefficient Q-H-realtion bsde
c                         (11,i)= coefficient Q-H-realtion csde
c                         (12,i)= coefficient Q-H-realtion dsde
c  7 strpar(21,nstru)  I  Each structure is characterized by a number of
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
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flupdo  FLow UP- and DOwnstream near structure
c kaawar
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flaw.pf,v $
c Revision 1.14  1999/03/15  15:49:30  kuipe_j
c tabs removed
c
c Revision 1.13  1998/02/13  12:12:29  kuipe_j
c Adapt to CMT
c
c Revision 1.12  1997/11/26  14:54:31  kuipe_j
c diffusion zero for free flow + width zero allowed
c
c Revision 1.11  1997/10/03  06:39:34  kuipe_j
c criterium for flow drection changed
c
c Revision 1.10  1996/04/12  13:03:39  kuipe_j
c headers, minor changes
c
c Revision 1.9  1996/04/11  08:23:09  kuipe_j
c Kalman module added
c
c Revision 1.8  1996/01/17  14:38:12  kuipe_j
c header update
c
c Revision 1.7  1995/09/22  10:00:55  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:10:47  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:36:24  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:11  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:54:44  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:40  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:27  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:30:29  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Declaration of function:
c
      real    FLQHAW
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      integer il, ir, istru, ngrid
      logical strclo(*)
      real    strpar(dmstrpar,*), strhis(dmstrh,*)
      double precisionh(ngrid), h1(ngrid), q(ngrid), q2(ngrid)
      real    af(ngrid)
      real    rho(ngrid)
      real    asde, csde, esde, g
c
c     Declaration of local variables:
c
      integer iup, idown
      logical strsta
      real    hunp1, hdnp1, hun, hdn, uu, ud, dh, qa, qdhu, qdhd, teken
      real    h0, ka, kp, n, p, wn, zs
      real    dum1, dum2, dum3, dum4, dum5, dum6, dum7, dum8, dum9,
     +        dum10, dum11, dum12, dum13, wideff,hcrfre
      real    crest

      crest = strpar(1 ,istru)
c
      call FLUPDO(g      ,il     ,ir     ,ngrid  ,h      ,h1     ,
     +            q      ,q2     ,af     ,rho    ,crest  ,hunp1  ,
     +            hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,
     +            idown  ,teken  )
c
c     Determine crest height, total net width and number of piers
c     Determine P, H0, Kp and Ka (flow direction dependent)
c
c
c      call KAAWAR(istru  ,strpar ,teken  ,h0     ,ka     ,kp     ,
c     +            n      ,p      ,wn     ,zs     )
c
      dh = 0.001
c
      strsta = .true.
      qa = teken * FLQHAW(g      ,istru  ,strsta ,strclo ,hunp1  ,
     +                    hdnp1  ,uu     ,h0     ,ka     ,kp     ,
     +                    n      ,p      ,wn     ,zs     ,wideff ,
     +                    dum2   ,dum3   ,dum4   ,dum5   ,dum6   ,
     +                    dum7   ,dum8   ,dum9   ,dum10  ,dum11  ,
     +                    dum12  ,dum13  )
c
      strhis(4,istru) = qa
      strsta = .false.
c
      if (teken .gt. 0) then
         qdhu = FLQHAW (g      ,istru  ,strsta ,strclo ,hunp1+dh,
     +                  hdnp1  ,uu     ,h0     ,ka     ,kp      ,
     +                  n      ,p      ,wn     ,zs     ,dum1    ,
     +                  dum2   ,dum3   ,dum4   ,dum5   ,dum6    ,
     +                  dum7   ,dum8   ,dum9   ,dum10  ,dum11   ,
     +                  dum12  ,dum13  )

         qdhu = ( qdhu - qa ) / dh
      else
         qdhu = FLQHAW (g      ,istru  ,strsta ,strclo ,hunp1   ,
     +                 hdnp1-dh,uu     ,h0     ,ka     ,kp      ,
     +                  n      ,p      ,wn     ,zs     ,dum1    ,
     +                  dum2   ,dum3   ,dum4   ,dum5   ,dum6    ,
     +                  dum7   ,dum8   ,dum9   ,dum10  ,dum11   ,
     +                  dum12  ,dum13  )
         qdhu = ( qdhu + qa ) / dh
      endif
c
      if (teken .gt. 0) then
         qdhd = FLQHAW (g      ,istru  ,strsta ,strclo ,hunp1   ,
     +                 hdnp1-dh,uu     ,h0     ,ka     ,kp      ,
     +                  n      ,p      ,wn     ,zs     ,dum1    ,
     +                  dum2   ,dum3   ,dum4   ,dum5   ,dum6    ,
     +                  dum7   ,dum8   ,dum9   ,dum10  ,dum11   ,
     +                  dum12  ,dum13  )
         qdhd = ( qa - qdhd ) / dh
      else
         qdhd = FLQHAW (g      ,istru  ,strsta ,strclo ,hunp1+dh,
     +                  hdnp1  ,uu     ,h0     ,ka     ,kp      ,
     +                  n      ,p      ,wn     ,zs     ,dum1    ,
     +                  dum2   ,dum3   ,dum4   ,dum5   ,dum6    ,
     +                  dum7   ,dum8   ,dum9   ,dum10  ,dum11   ,
     +                  dum12  ,dum13  )
         qdhd = ( -qdhd - qa)/dh
      endif
c
c     Calculate if structure is drowned or free
c
      if (wideff .le. 1.e-6) then
         strclo(istru) = .true.
      endif
      if (strclo(istru)) then
         strhis(8,istru) = 0.
      else
         hcrfre = (hunp1 + uu**2/2./g - zs) * .6666666
         if (hcrfre .lt. (hdnp1-zs)) then
c            drowned weir flow
             strhis(8,istru) = 2.
         else 
c            free weir flow
             strhis(8,istru) = 1.
         endif
      endif
c
      asde = qdhu
c
      csde = qdhd
c
      if (teken .gt. 0) then
         esde = -qa + (hunp1-hun) * qdhu + (hdnp1-hdn) * qdhd
      else
         esde = -qa + (hdnp1-hdn) * qdhu + (hunp1-hun) * qdhd
      endif
c
      end
