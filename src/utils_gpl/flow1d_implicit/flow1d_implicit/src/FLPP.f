      subroutine FLPP (il     ,ir     ,ngrid  ,istru  ,strclo ,strpar ,
     +                 h      ,h1     ,maxtab ,ntabm  ,ntab   ,table  ,
     +                 strhis ,asde   ,csde   ,esde   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLPP (FLow structure PumP)
c
c Module description: In subroutine FLPP the ABCDE coefficients are
c                     computed for a pump.
c
c                     In this subroutine the coefficients A2-E2 will be
c                     computed for the stage-discharge equation for this
c                     specific structure. In subroutine FLQHPP the Q-H
c                     relation for the pump is defined.
c
c                     The pump is controlled by the upward or downward
c                     water level. By definition the upward side is the
c                     side from the beginning of the branch, the down-
c                     ward side points to the end of the branch.
c
c                     The reduction factor of the pump capacity can be
c                     given as a function of the head.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 14 asde              O  a-coefficient in stage-discharge equation for
c                         this specific structure.
c 15 csde              O  c-coefficient in stage-discharge equation for
c                         this specific structure.
c 16 esde              O  e-coefficient in stage-discharge equation for
c                         this specific structure.
c  8 h1(ngrid)         I  Water level in every grid point at time t(n).
c  7 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  1 il                I  Grid point on left side of structure (lower
c                         index).
c  2 ir                I  Grid point on right side of structure (upper
c                         index).
c  4 istru             I  Number of structure.
c  9 maxtab            I  Maximum number of defined tables.
c  3 ngrid             I  Number of grid points in network.
c 11 ntab              P  -
c 10 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  5 strclo            P  -
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
c  6 strpar(21,nstru)  I  Each structure is characterized by a number of
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
c 12 table             P  -
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flpp.pf,v $
c Revision 1.9  1999/03/15  15:50:26  kuipe_j
c tabs removed
c
c Revision 1.8  1998/02/13  12:12:30  kuipe_j
c Adapt to CMT
c
c Revision 1.7  1997/11/26  14:54:33  kuipe_j
c diffusion zero for free flow + width zero allowed
c
c Revision 1.6  1996/01/17  14:38:43  kuipe_j
c header update
c
c Revision 1.5  1995/09/22  10:02:06  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/30  12:36:47  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:19  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:19  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:00  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:31:23  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:53  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of function:
c
      real    FLQHPP
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      integer il, ir, istru, ngrid, maxtab, ntabm, ntab(4,maxtab)
      logical strclo(*)
      real    strpar(dmstrpar,*), strhis(dmstrh,*)
      double precision h(ngrid), h1(ngrid)
      real    table(ntabm)
      real    asde, csde, esde
c
c     Declaration of local variables:
c
      logical strsta
      integer itab ,iup  ,idown
      real    hact ,dh   ,qa    ,qdhu ,qdhd  ,cap  ,capold ,hstart , 
     +        hstop, teken
      double precision hup  ,hdown
c
c     Assign value strpar to local variable:
c
      call FLPPAR(istru  ,strpar ,ngrid  ,h1     ,il     ,ir     ,
     +            itab   ,cap    ,capold ,hstart ,hstop  ,
     +            iup    ,idown  ,teken  ,hact   )
c
      dh = 0.01
c
      strsta = .true.
      qa = FLQHPP (istru  ,strsta ,strclo ,h(iup) ,h(idown) ,
     +             hact   ,itab   ,cap    ,capold ,hstart ,hstop    ,
     +             maxtab ,ntabm  ,ntab   ,table  ,teken  )
c
c
c     Save old capacity in strpar
c     Function return value is discharge Q for pump
c
      strpar(6,istru) = capold
      strhis(4,istru) = qa
      strhis(8,istru) = 1.
      strsta = .false.
c
c     Differentiation to i-left
c
      if (teken .gt. 0.) then
         hup   = h(iup)   + dh
         hdown = h(idown)
      else
         hup   = h(iup)
         hdown = h(idown) - dh
      endif  
      qdhu = FLQHPP (istru   ,strsta ,strclo ,hup    ,hdown  ,
     +               hact    ,itab   ,cap    ,capold ,hstart ,hstop  ,
     +               maxtab  ,ntabm  ,ntab   ,table  ,teken)
c
      qdhu = (qdhu - qa)/dh*teken
c
c
c     Differentiation to i-right
c
      if (teken .gt. 0.) then
         hup   = h(iup)
         hdown = h(idown) - dh
      else       
         hup   = h(iup)   + dh
         hdown = h(idown)
      endif  
      qdhd = FLQHPP (istru   ,strsta ,strclo ,hup    ,hdown  ,
     +               hact    ,itab   ,cap    ,capold ,hstart ,hstop  ,
     +               maxtab  ,ntabm  ,ntab   ,table  ,teken  )
c
      qdhd = (qa - qdhd)/dh*teken
c
      asde = qdhu
c
      csde = qdhd
c
      esde = -qa + ( h(il)-h1(il) )*qdhu + ( h(ir)-h1(ir) )*qdhd
c
      end
