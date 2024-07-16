      subroutine KADQLT(g      ,time   ,ngrid  ,x      ,h1     ,h      ,
     +                  strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,
     +                  nqlat  ,qltpar ,juer   ,lambda ,theta2 ,dt     ,
     +                  qlat   ,strclo ,qlatac ,dqltdh ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KADQLT (KAlman Derivative of Q LaTeral)
c
c Module description: Computation of the derivatives of the waterlevel
c                     dependent lateral discharges in the user selected
c                     discharge stations at time level t(n+1/2).
c
c                     Calculate the lateral discharges for an incremented
c                     water level using routine FLQLAT. Use these values
c                     and the lateral discharges obtained in the flow
c                     module to determine the derivatives by numerical
c                     differentiation.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 22 dqltdh(ngrid)     O  Derivative of lateral discharge to waterlevel in
c                         every grid point i+1/2 on time n+1/2
c                         (d(Qlat)/dh). Value at i+1/2 is stored at index
c                         i.
c 18 dt                I  Computational time step dt [sec].
c  1 g                 I  Acceleration of gravity.
c  5 h1(ngrid)         I  Water level in every grid point at time t(n).
c  6 h(ngrid)          I  Contains water levels in every grid point. It is
c                         stored on index 2 of the packed array hpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c 15 juer              P  -
c 23 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 16 lambda            I  Extra resistance in general structure.
c  9 maxtab            I  Maximum number of defined tables.
c  3 ngrid             I  Number of grid points in network.
c 13 nqlat             I  Number of lateral discharge stations.
c 11 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c 10 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 19 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
c                         time level n+1/2.
c 21 qlatac(nqlat)     IO Lateral discharge in every lateral discharge
c                         location i on time n+1/2+dh.
c 14 qltpar(9,nqlat)   I  Defines lateral discharge stations (Qlat):
c                         (1,i) = Branch number.
c                         (2,i) = Type of Qlateral definition:
c                                 cqlftm (1) : Qlat = f(t)
c                                 cqlfh  (2) : Qlat = Q(h)
c                                 cqlstr (3) : Qlat from structure
c                                 cqlcon (4) : Qlat from other lateral
c                                              discharge station
c                         - For types 1 and 2 (functns of time or Q(h)):
c                         (3,i) = Table number.
c                         (4,i) = Point or traject type:
c                                 cpd1gp (1) : Point discharge 1 gpoint
c                                 cpd1gc (2) : Point discharge 1 cell
c                                 ctd1gc (3) : Traject discharge 1 cell
c                                 ctdmgc (4) : Traject discharge over
c                                              more grid cells
c                         (5,i) = First gridpoint of cell/trajectory
c                                 (types 1,2,3,4).
c                         (6,i) = Last gridpoint of cell/trajectory
c                                 (types 2,3,4).
c                         (7,i) = Lb coordinate for cell/trajectory
c                                 (types 3,4).
c                         (8,i) = Le coordinate for cell/trajectory
c                                 (types 3,4).
c                         - For type 3 (structure):
c                         (3,i) = Table number of outside water level
c                                 table.
c                         (4,i) = Point or traject type:
c                                 cpd1gp (1) : Point discharge 1 gpoint
c                                 cpd1gc (2) : Point discharge 1 cell
c                         (5,i) = First gridpoint of cell.
c                         (6,i) = Last gridpoint of cell.
c                         (7,i) = Structure type.
c                         (8,i) = Structure number.
c                         - For type 4 (connection point):
c                         (3,i) = Second index of qltpar which is the
c                                 connection point.
c                         (4,i) = Point or traject type:
c                                 cpd1gp (1) : Point discharge 1 gpoint
c                                 cpd1gc (2) : Point discharge 1 cell
c                         (5,i) = First gridpoint of cell.
c                         (6,i) = Last gridpoint of cell.
c                         Below a drawing is given which defines the
c                         locations of the grid points and trajectory
c                         (Lb , Le):
c                            x=Lb                    x=Le
c                             |                       |
c                         -+-------+--------------+--------+-----
c                         i1  |   i1+1            i2  |   i2+1
c 20 strclo            P  -
c  8 strpar            P  -
c  7 strtyp(10,nstru)  I  Structure definitions:
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
c 12 table             P  -
c 17 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c  2 time              I  Actual time level tn+1. in sec.
c  4 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c cqlatg  Calculate Q LaTeral per Grid point
c error   write an ERROR to the error file.
c flgtar  FLow get General sTructure ARguments
c flhavg  FLow AVeraGed waterlevel H
c flswar  FLow get Simple Weir ARguments
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kadqlt.pf,v $
c Revision 1.3  1999/03/15  15:51:39  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:04:45  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:22  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Function declaration
c
      real FLQHSW
      double precision FLQHGS
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer ngrid, maxtab, ntabm, nqlat, juer, ker
      integer strtyp(10,*), ntab(4,maxtab)

      logical strclo(*)

      real    lambda   , theta2
      real    g, qltpar(9,*), qlat(*), qlatac(*), dqltdh(ngrid),
     +        x(ngrid), table(ntabm), strpar(dmstrpar,*)

      double  precision  time, dt, h1(ngrid), h(ngrid)
c
c     Declaration of local variables
c
      character txt*4
      logical   strsta
      integer   igr, iopt, istat, istat2, istru, itab, i1, i2, type
      integer   idum
      real      hup, hdown, hin, hout, lbt, let, hstat, qstat, sign
      real      uu, ud, zs, wstr, cw, slim
      real      dh, dqlat, fred
      double precision w2,   wsd,  zb2, ds1, ds2,   rhoast,
     +                 cgf,  cgd,  cwf, cwd, mugf,  dhstru, zsg,
     +                 wstrg,dg,   tpth2,    dum
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'
c
      parameter (dh = 0.001)
c
c     ***********************************
c     * Computation of Qlat in stations *
c     ***********************************
c
      do 10 istat = 1, nqlat
         qlatac(istat) = qlat(istat)
c
c        iopt = 2 : Qlat follows from Q(h)-table
c             = 3 : Qlat follows from structure
c             = 4 : Qlat follows from lateral discharge in 2nd lateral
c                   discharge station
c
         iopt = int(qltpar(2,istat))
c
         if (iopt .eq. cqlfh) then
c
c           **********************************************
c           * Qlat from Q(h)-table              (iopt=2) *
c           **********************************************
c
c           itab  : TABLE number for Q(h)-table
c           hstat : water level h in Qlat station
c
            itab  = int(qltpar(3,istat))
            type  = int(qltpar(4,istat))
c
c           type:
c           1. point discharge in one gridpoint
c           2. point discharge in grid cell
c           3. traject discharge in grid cell
c           4. traject discharge over more grid cells
c
            if (type .eq. cpd1gp) then

               igr   = int(qltpar(5,istat))
               hstat = theta2 * h(igr) + (1. - theta2) * h1(igr)

            else if (type .eq. cpd1gc) then

               igr   = int(qltpar(5,istat))
               hstat = theta2 * (h(igr) + h(igr+1)) / 2. +
     +                (1. - theta2) * (h1(igr) + h1(igr+1)) / 2.

            else if (type .eq. ctd1gc .or. type .eq. ctdmgc) then

               i1  = int(qltpar(5,istat))
               i2  = int(qltpar(6,istat))
               lbt = qltpar(7,istat)
               let = qltpar(8,istat)
               call FLHAVG (ngrid, x, h1, h, lbt, let, i1, i2, hstat,
     +                      theta2)

            endif

            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
     +                   dble(hstat+dh) , qstat      )

            qlatac(istat) = qstat

         else if (iopt .eq. cqlfst) then
c
c           **********************************************
c           * Qlat from structure               (iopt=3) *
c           **********************************************
c
c           istru : number of structure in Qlat station
c           hstat : water level h in nearest gridpoint
c           hout  : outside water level
c           itab  : TABLE number table hout=f(t)
c           time  : t(n+1/2)
c
            istru = int(qltpar(8,istat))
            itab  = int(qltpar(3,istat))
c
            igr   = int(qltpar(5,istat))
            hin   = theta2 * (h(igr) + h(igr+1)) / 2. +
     +              (1. - theta2) * (h1(igr) + h1(igr+1)) / 2. + dh
            tpth2 = dble (theta2 * time + (1. - theta2) * (time - dt))
c
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
     +                   tpth2, hout   )
c
c           Determine flow direction, upstream/downstream water level
c           and bottom downstreams the structure
c
            if ( hout .gt. hin ) then
               hup   = hout
               hdown = hin
               sign  = 1.0
            else
               hup   = hin
               hdown = hout
               sign  = -1.0
            endif
            uu = 0.
            ud = 0.
c
            if ( strtyp(1,istru) .eq. csweir ) then
c
c              ***************
c              * Simple weir *
c              ***************
c              Remark:
c              function FLQHSW incorporates the stage-discharge
c              relation for a simple weir
c
               call FLSWAR (istru  ,strpar ,sign   ,zs     ,
     +                      wstr   ,cw     ,slim   ,itab   )
c
               strsta = .false.
               qlatac(istat) = FLQHSW(g      ,istru  ,strsta ,
     +                                strclo ,hup    ,hdown  ,uu     ,
     +                                ud     ,zs     ,wstr   ,cw     ,
     +                                slim   ,itab   ,maxtab ,ntabm  ,
     +                                ntab   ,table  ,fred   ) * sign
            else if ( strtyp(1,istru) .eq. cgenst ) then
c
c              *********************
c              * General Structure *
c              *********************
c              Remark:
c              function FLQHGS incorporates the stage-discharge
c              relation for a general structure
c
               call FLGTAR (istru  ,strpar ,dble(sign)     ,zsg    ,
     +                      wstrg  ,w2     ,wsd    ,zb2    ,dg     ,
     +                      ds1    ,ds2    ,cgf    ,cgd    ,cwf    ,
     +                      cwd    ,mugf   )
c
               strsta = .false.
               rhoast = 1.0D0
               dhstru = 0.0D0
               qlatac(istat) = sngl (FLQHGS (
     +                         dble(g)      ,istru ,strsta ,strclo  ,
     +                         dble(hup)    ,dble(hdown)   ,dble(uu),
     +                         zsg   ,wstrg ,w2    ,wsd    ,zb2     ,
     +                         dg    ,ds1   ,ds2   ,rhoast ,cgf     ,
     +                         cgd   ,cwf   ,cwd   ,mugf   ,dum     ,
     +                         idum  ,dble(lambda) ,dhstru )
     +                         ) * sign
            else
c
c              ERROR Unexpected type for lateral structure
c
               ker = fatal
               write (txt,'(i4)') istru
               call ERROR (juer,'KADQLT  structure number @'//txt//'@',
     +                           eflstr, ker)
               goto 1000
            endif
         endif

   10 continue
c
      do 20 istat = 1, nqlat
c
         iopt = int(qltpar(2,istat))
c
         if (iopt .eq. cqlcon) then
c
c           **********************************************
c           * Qlat from other discharge station (iopt=4) *
c           **********************************************
c
c           istat2       = number of 2nd Qlat station
c           qlat(istat2) = Qlat at 2nd station at previous time
c                         level (explicit coupling)
c
            istat2      = int(qltpar(3,istat))
            qlatac(istat) = -qlatac(istat2)
         endif
   20 continue

c
c     *****************************************
c     * Distribution of Qlat over grid points *
c     *****************************************
c
      do 30 igr = 1, ngrid
         dqltdh(igr) = 0.
   30 continue
c
      do 40 istat = 1, nqlat
         dqlat = ( qlatac(istat) - qlat(istat) ) / dh
         call CQLATG (ngrid  ,nqlat  ,istat  ,dqlat  ,qltpar ,
     +                x      ,dqltdh )
   40 continue
c
 1000 continue
      end
