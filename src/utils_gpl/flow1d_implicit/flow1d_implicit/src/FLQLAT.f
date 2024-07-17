      subroutine FLQLAT(g      ,time   ,ngrid  ,lambda ,x      ,h1     ,
     +                  h      ,strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,
c                       mozart parameters
     +                  lmoza  ,istmoz ,qlatid ,
     +                  table  ,nqlat  ,qltpar ,juer   ,qlat   ,qlatgr ,
     +                  strclo ,strhis ,theta2 ,dt     ,ker    ,omqlat ,
     +                  dhstru ,relstr ,iter)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLQLAT (FLow Q LATeral)
c
c Module description: In subroutine FLQLAT first the lateral discharges
c                     Qlat will be computed for each station. After this
c                     the calculated values will be distributed over the
c                     points i+1/2.
c
c                     The computation of the lateral discharges can be
c                     split up in the computation of the discharges per
c                     station, followed by the distribution of this
c                     discharge. To one point more than one lateral
c                     discharge may be related. For this reason the
c                     distributed lateral discharges in the points i+1/2
c                     have to be accumulated rather than assigned.
c
c                     The discharge for a station can be defined as:
c
c                     1.  Qlat as time series Qlat (t);
c                     2.  Qlat follows from a Q(h) table;
c                     3.  Qlat follows from structure;
c                     4.  Qlat follows from 2nd lateral discharge sta-
c                         tion;
c                     5.  Qlat defined from water level in retention
c                         area.
c
c                     Options 1, 2 and 5 are to be given as a point
c                     lateral discharge Qlat or on a trajectory as qlat.
c                     Options 3 and 4 are always a point lateral dis-
c                     charge (Qlat).
c
c                     After the computation of the lateral discharge
c                     Qlat in the station, the discharge will be dis-
c                     tributed over the points i+1/2.
c
c                     Point lateral discharge distribution:
c
c                         For a point lateral discharge in grid point i
c                         the discharge is distributed over the points
c                         i-1/2 and i+1/2. (form. 6-4 a/b ,S-FO-
c                         001.5KV).
c
c                         For a point lateral discharge in a grid cell
c                         the lateral discharge for point i+1/2 follows
c                         from formula 6-4 c ,S-FO-001.5KV.
c
c                     Trajectory lateral discharge distribution:
c
c                     For a lateral discharge over a trajectory
c                     [Lbt,Let] there are two separate cases to distin-
c                     guish:
c
c                         - Trajectory within one grid cell
c
c                         Discharge in point i+1/2 follows from formula
c                         6-6 ,S-FO-001.5KV.
c
c                         - Trajectory over more than one grid cell
c
c                         Several formulas are used here:
c
c                         -   Begin of trajectory (part of grid cell
c                             enclosed) : formula 6-5 a ,S-FO-001.5KV .
c                         -   End of trajectory (part of grid cell
c                             enclosed) : formula 6-5 b ,S-FO-001.5KV .
c                         -   Enclosed grid cells: lateral discharge
c                             equals qlat
c
c                     Structures:
c
c                     One of the possibilities to define a lateral dis-
c                     charge is to use a structure. Only the simple weir
c                     and simple gate can be used. To calculate the lat-
c                     eral discharge the QH relation routines for these
c                     structures will be called. These subroutines need
c                     two water levels. One of the water levels follow
c                     from the grid point where the lateral discharge
c                     has been defined. The other water follows from a
c                     user given table (hout(side)).
c
c                     The simple weir subroutine also needs a velocity
c                     to calculate the energy level. The velocity how-
c                     ever is unknown and is therefore set to zero. The
c                     formulas will operate with water levels in stead
c                     of energy levels.
c
c                     Q(h) table
c
c                     The possibility for a Q(h) table can be point
c                     lateral or on a trajectory. In case a trajectory
c                     has been specified an averaged water level or
c                     depth over the trajectory is calculated in the
c                     routine FLHAVG.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 22 dt                I  Computational time step dt [sec].
c  1 g                 I  Acceleration of gravity.
c  6 h1(ngrid)         I  Water level in every grid point at time t(n).
c  7 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c    iter              I  Iteration step.
c 16 juer              P  -
c 23 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  4 lambda            I  Extra resistance in general structure.
c 10 maxtab            I  Maximum number of defined tables.
c  3 ngrid             I  Number of grid points in network.
c 14 nqlat             I  Number of lateral discharge stations.
c 12 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c 11 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 24 omqlat            I  underrelaxation parameter omega for lateral
c                         discharges
c 17 qlat(nqlat)       IO (i) = Actual lateral discharge in station i on
c                         time level n+1/2.
c 18 qlatgr(ngrid)     O  (i) = Actual lateral discharge in grid point
c                         i+1/2.
c 15 qltpar(9,nqlat)   I  Defines lateral discharge stations (Qlat):
c                         (1,i) = Branch number.
c                         (2,i) = Type of Qlateral definition:
c                                 cqlftm (1) : Qlat = f(t)
c                                 cqlfh  (2) : Qlat = Q(h)
c                                 cqlstr (3) : Qlat from structure
c                                 cqlcon (4) : Qlat from other lateral
c                                              discharge station
c                                 cqlret (5) : Qlat from retention area
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
c                         - For type 5 (retention area):
c                         (3,i) = Retention area in m^2
c                         (4,i) = Point or traject type:
c                                 cpd1gp (1) : Point discharge 1 gpoint
c                                 cpd1gc (2) : Point discharge 1 cell
c                         (5,i) = First gridpoint of cell.
c                         (6,i) = Last gridpoint of cell.
c                         (7,i) = Bed level in retention area
c                         (8,i) = Water level in retention area at start
c                                 of timestep
c                         (9,i) = Structure number, ist, where:
c                                 ist = is1 + 1000*is2
c                                 is1 = first structure
c                                 is2 = second structure (optional)
c 19 strclo            P  -
c 20 strhis(13,nstru)  O  For each structure the discharge and the
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
c                         (13,i)= water level in retention area
c  9 strpar            P  -
c  8 strtyp(10,nstru)  I  Structure definitions:
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
c 13 table             P  -
c 21 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c  2 time              I  Actual time level tn+1. in sec.
c  5 x                 P  -
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
c $Log: flqlat.pf,v $
c Revision 1.13  1999/06/01  13:42:20  kuipe_j
c names in messages substituted + message template
c
c Revision 1.12  1999/03/15  14:24:42  kuipe_j
c Bug fix lateral discharge on t=0
c
c Revision 1.11  1997/11/04  14:17:28  kuipe_j
c Retention basin
c
c Revision 1.10  1997/01/23  08:29:16  kuipe_j
c Make flow module robust
c
c Revision 1.9  1996/04/12  13:04:19  kuipe_j
c headers, minor changes
c
c Revision 1.8  1996/04/11  08:23:52  kuipe_j
c Kalman module added
c
c Revision 1.7  1996/01/17  14:38:45  kuipe_j
c header update
c
c Revision 1.6  1995/09/22  10:02:10  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:11:01  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:36:50  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:24  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:24  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:04  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:50  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:28  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:54  kuipe_j
c Initial version
c
c
c***********************************************************************
c

c      !DEC$ IF DEFINED (_DLL)
c      use SobekRE_OpenMI
c      !DEC$ ENDIF
c
c     Function declaration
c
      real             FLQHSW
      double precision FLQHGS
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
      include '../include/mempool.i'
c
c     Declaration of parameters
c
      integer ngrid, maxtab, ntabm, nqlat, juer, ker
      integer strtyp(10,*), ntab(4,maxtab), iter
c     Koppeling Mozart
      integer istmoz
      logical lmoza
      character*40 qlatid(*)
c
c
      logical strclo(*)
      real    lambda   , theta2 , omqlat, dhstru, relstr
      real    g, qltpar(9,*), qlat(nqlat,9), qlatgr(ngrid),
     +        x(ngrid), table(ntabm),
     +        strpar(dmstrpar,*), strhis(dmstrh,*)
      double  precision  time
      double  precision  dt, dhsdub
      double  precision  h1(ngrid), h(ngrid)
c
c     Declaration of local variables
c
      character strnam*40
      logical   strsta
      integer   igr, iopt, istat, istat2, istru, itab, i1, i2, type
      integer   formno ,lstnam, qlatnm
      integer   is1, is2, istrc, nstrc
      real      hup, hdown, hin,  hout, lbt, let, hstat, qstat,  teken
      real      uu,  ud,    zs,   wstr, cw,  slim,dum1 , qltstr
      real      hret, ires
      real      astr, bstr, cstr, estr, area, area1, width
      real      as(2), bs(2), cs(2), es(2), help
      real      w1, zb1, qltpls, qltmin, dqdhup, dqdhdn
      double precision      w2,   wsd,  zb2, ds1, ds2,   rhoast,
     +                      cgf,  cgd,  cwf, cwd, mugf,  zsg,
     +                      wstrg,dg,   tpth2,    dum2
c
c     External functions
c
      integer gtcpnt
c
c     Koppeling Mozart
      integer  , parameter ::  moztyp=8
c
c     Include sobek error code file
c
      include '../include/errcod.i'
      include '../include/sobcon.i'
c
c     ***********************************
c     * Computation of Qlat in stations *
c     ***********************************
c
      dhsdub = dhstru
      do 10 istat = 1, nqlat
c
c        iopt = 1 : Qlat defined as time series
c             = 2 : Qlat follows from Q(h)-table
c             = 3 : Qlat follows from structure
c             = 4 : Qlat follows from lateral discharge in 2nd lateral
c                   discharge station
c             = 5 : Qlat determined from water level in retention
c                   area
c
         iopt = int(qltpar(2,istat))
c
c JC 12/4/2000: oorspr. Mozart aanpassingen verwijderd uit onderstaande
         if (iopt .eq. cqlftm) then
c
c           **********************************************
c           * Qlat defined as a function of time (iopt=1)*
c           **********************************************
c
c           itab : TABLE number table Qlat=f(t)
cc          time : t(n+1/2)
c           time : t(n+1)
c
            itab  = int(qltpar(3,istat))
            tpth2 = dble (theta2 * time + (1. - theta2) * (time - dt))

            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
     +                   tpth2,qstat       )
c
c           Lataral discharge with underrelaxation
c
            if ( qlat(istat,1) .gt. 1.1E+20 ) then
               qlat(istat,1) = qstat
            else
               qlat(istat,1) = omqlat*qstat+(1.-omqlat)*qlat(istat,1)
            endif

         else if (iopt .eq. cqlfh) then
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
     +                     theta2)
c
            endif

            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
     +                   dble(hstat) , qstat      )
c
c           Lataral discharge with underrelaxation
c
            if ( qlat(istat,1) .gt. 1.1E+20 ) then
               qlat(istat,1) = qstat
            else
               qlat(istat,1) = omqlat*qstat+(1.-omqlat)*qlat(istat,1)
            endif

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
cc          time  : t(n+1/2)
c           time  : t(n+1)
c
           istru   = int(qltpar(8,istat))
           itab    = int(qltpar(3,istat))
           type    = int(qltpar(4,istat))
c
           if (type .eq. cpd1gp) then

              igr   = int(qltpar(5,istat))
              hin   = theta2 * h(igr) + (1. - theta2) * h1(igr)

           else if (type .eq. cpd1gc) then

              igr   = int(qltpar(5,istat))
              hin   = theta2 * (h(igr) + h(igr+1)) / 2. +
     +                (1. - theta2) * (h1(igr) + h1(igr+1)) / 2.

           endif
           tpth2 = dble (theta2 * time + (1. - theta2) * (time - dt))

           call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
c    +                   time, hout   )
     +                   tpth2,hout   )
c
c           Determine flow direction, upstream/downstream water level
c           and bottom downstreams the structure
c
           if ( hout .gt. hin ) then
              hup   = hout
              hdown = hin
              teken = 1.0
           else
              hup   = hin
              hdown = hout
              teken = -1.0
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
               call FLSWAR (istru  ,strpar  ,teken   ,zs     ,
     +                      wstr   ,cw     ,slim   ,itab   )
c
               strsta = .true.
               qltstr = FLQHSW(g      ,istru  ,strsta ,
     +                         strclo ,hup    ,hdown  ,uu     ,
     +                         ud     ,zs     ,wstr   ,cw     ,
     +                         slim   ,itab   ,maxtab ,ntabm  ,
     +                         ntab   ,table  ,dum1 ) *  teken
c
c           Lataral discharge with underrelaxation
c

               if ( qlat(istat,1) .gt. 1.1E+20 ) then
                  qlat(istat,1) = qltstr
               else
                  qlat(istat,1) = omqlat*qltstr
     &                          + (1.-omqlat)*qlat(istat,1)
               endif
c
               strhis(4,istru) = qlat(istat,1)
               strsta = .false.
            else if ( strtyp(1,istru) .eq. cgenst ) then
c
c              *********************
c              * General Structure *
c              *********************
c              Remark:
c              function FLQHGS incorporates the stage-discharge
c              relation for a general structure
c
               call FLGTAR (istru  ,strpar ,dble(teken)    ,zsg    ,
     +                      wstrg  ,w2     ,wsd    ,zb2    ,dg     ,
     +                      ds1    ,ds2    ,cgf    ,cgd    ,cwf    ,
     +                      cwd    ,mugf   )
c
               strsta = .true.
               rhoast = 1.0D0
               qltstr = sngl(FLQHGS (dble(g),istru  ,strsta ,
     +                       strclo         ,dble(hup)      ,
     +                       dble(hdown)    ,dble(uu)       ,
     +                       zsg    ,wstrg  ,w2     ,wsd    ,
     +                       zb2    ,dg     ,ds1    ,ds2    ,
     +                       rhoast ,cgf    ,cgd    ,cwf    ,
     +                       cwd    ,mugf   ,dum2   ,formno ,
     +                       dble(lambda)   ,dhsdub )
     +                     * teken)
c
c           Lataral discharge with underrelaxation
c
               if ( qlat(istat,1) .gt. 1.1E+20 ) then
                  qlat(istat,1) = qltstr
               else
                  qlat(istat,1) = omqlat*qltstr
     &                          + (1.-omqlat)*qlat(istat,1)
               endif
c
               strhis(8,istru) = formno
               strhis(4,istru) = qlat(istat,1)
               strsta = .false.
           else
c
c              ERROR Unexpected type for lateral structure
c
              ker = fatal
c              call getstr(istru,strnam,lstnam)
c              call sre_error (juer,'FLQLAT  structure number@'//
c     +        strnam(:lstnam)//'@',eflstr, ker)
              goto 1000
            endif
         else if ( iopt .eq. cqlret ) then
c
c
c
c           **************************************************
c           * Qlat from retention area (iopt = 5)            *
c           **************************************************
c
c           determine structure ids and discharge type
c
            is1 = MOD(INT(qltpar(9,istat)), 1000)
            is2 = INT(qltpar(9,istat))/1000
            type = INT(qltpar(4,istat))
c
c           Set water level and discharge for the first iteration step
c           to the values of the previous time step N+1
c
            if (iter .eq. 1) then
c             Hr_n+1,m          Hr_N+1
              qltpar(8,istat) = strhis(13,is1)
c             Hr_n              Hr_N+1
              qlat(istat,9)   = strhis(13,is1)
              if ( qlat(istat,1) .gt. 1.1E+20 ) qlat(istat,1) = 0.
c             Qr_n              Qr_N+1
              qlat(istat,3)   = qlat(istat,1)
c             Qr_n+1,m-1        Qr_N+1
              qlat(istat,2)   = qlat(istat,1)
            endif

c
c           determine water level difference
c           ### this should be replaced later as qlat mst be in a
c               cell and not on a grid point ###
c
            if (type .eq. cpd1gc) then
               igr   = int(qltpar(5,istat))
c               hstat = theta2 * (h(igr) + h(igr+1)) / 2. +
c     +                 (1. - theta2) * (h1(igr) + h1(igr+1)) / 2.
               hstat = (h(igr) + h(igr+1)) / 2.
            else
c
c             ERROR Retention areas cannot be defined on grid points
c
              ker = fatal
c              call getstr(is1,strnam,lstnam)
c              call sre_error (juer,'FLQLAT structure@'//strnam(:lstnam)//
c     +                   '@ must be in a grid cell', eflqlt, ker)
              goto 1000
            endif
c                  Hr_n+1,m
            hret = qltpar(8,istat)
c
            if ( hret .gt. hstat ) then
               hup   = hret
               hdown = hstat
               teken = 1.0
            else
               hup   = hstat
               hdown = hret
               teken = -1.0
            endif
c
c           initialise structure coefficients
c           a, c and e are simply added for each structure
c           b uses a weighting. There is no d term.
c
            astr  = 0.0
            bstr  = 0.0
            cstr  = 0.0
            estr  = 0.0
c
c           loop over one or two structures
c
            nstrc = 1
            if (is2 .ne. 0) nstrc = 2

            do 50 istrc = 1, nstrc

               if (istrc .eq. 1) then
                  istru = is1
               else
                  istru = is2
               endif
c
               if ( strtyp(1,istru) .eq. cgenst ) then
c
c                 *********************
c                 * General Structure *
c                 *********************
c

c                 Remark: It is assumed that one structure
c                 is for positive flow and one for negative flow
c                 So the areas and widths should not be added.
c
                  area = 0.
                  width = 0.

                  if (teken .gt. 0.0) then
                     w1  = strpar( 1, istru)
                     zb1 = strpar( 2, istru)
                     if (hup .le. zb1) then
                        area1 = 0.0
                     else
                        area1 = (hup - zb1) * w1
                     endif
                  else
                     w1    = strpar( 9, istru)
                     zb1   = strpar(10, istru)
                     if (hup .le. zb1) then
                        area1 = 0.0
                     else
                        area1 = (hup - zb1) * w1
                     endif
                  endif
                  area = area + area1
                  width = width + w1
c
               else
c
c                 ERROR Unexpected type for lateral structure
c
                  ker = fatal
c                  call getstr(istru,strnam,lstnam)
c                  call sre_error (juer,
c     +                       'FLQLAT  structure number @'//
c     +                        strnam(:lstnam)//'@',
c     +                        eflstr, ker)
                  goto 1000
               endif
 50         continue
c
c            Water level of retention area is assumed to be
c            equal to upstream energy height in case of flow
c            from retention area
c
            area = max (area,.1)
            if (teken .gt. 0.0) then
               uu = 0.0
            else
               uu   = qlat(istat,1) / area
c               uu = 0.0
            endif
c
            do 55 istrc = 1, nstrc

               if (istrc .eq. 1) then
                  istru = is1
               else
                  istru = is2
               endif
c
               if ( strtyp(1,istru) .eq. cgenst ) then
c
c                 *********************
c                 * General Structure *
c                 *********************
c                 Remark:
c                 function FLQHGS incorporates the stage-discharge
c                 relation for a general structure
c
                  if (teken .gt. 0.0) then
                     w1  = strpar( 1, istru)
                  else
                     w1  = strpar( 9, istru)
                  endif
c
                  call FLGTAR (istru  ,strpar ,dble(teken)    ,zsg    ,
     +                         wstrg  ,w2     ,wsd    ,zb2    ,dg     ,
     +                         ds1    ,ds2    ,cgf    ,cgd    ,cwf    ,
     +                         cwd    ,mugf   )
c
                  strsta = .true.
                  rhoast = 1.0D0
                  qltstr = sngl(FLQHGS (dble(g),istru  ,strsta ,
     +                          strclo         ,dble(hup)      ,
     +                          dble(hdown)    ,dble(uu)       ,
     +                          zsg    ,wstrg  ,w2     ,wsd    ,
     +                          zb2    ,dg     ,ds1    ,ds2    ,
     +                          rhoast ,cgf    ,cgd    ,cwf    ,
     +                          cwd    ,mugf   ,dum2   ,formno ,
     +                          dble(lambda)   ,dhsdub )
     +                        * teken)
c
                  strhis(8,istru) = formno
                  strhis(4,istru) = qltstr
                  strsta = .false.
c
                  qltstr = sngl(FLQHGS (dble(g),istru  ,strsta ,
     +                          strclo         ,dble(hup)+dhsdub,
     +                          dble(hdown)    ,dble(uu)       ,
     +                          zsg    ,wstrg  ,w2     ,wsd    ,
     +                          zb2    ,dg     ,ds1    ,ds2    ,
     +                          rhoast ,cgf    ,cgd    ,cwf    ,
     +                          cwd    ,mugf   ,dum2   ,formno ,
     +                          dble(lambda)   ,dhsdub )
     +                        * teken)
                  qltpls = qltstr
                  dqdhup = (qltpls - strhis(4,istru)) / dhstru
c
                  qltstr = sngl(FLQHGS (dble(g),istru  ,strsta ,
     +                          strclo         ,dble(hup)      ,
     +                          dble(hdown)-dhsdub,dble(uu)       ,
     +                          zsg    ,wstrg  ,w2     ,wsd    ,
     +                          zb2    ,dg     ,ds1    ,ds2    ,
     +                          rhoast ,cgf    ,cgd    ,cwf    ,
     +                          cwd    ,mugf   ,dum2   ,formno ,
     +                          dble(lambda)   ,dhsdub )
     +                        * teken)
                  qltmin = qltstr
                  dqdhdn = (strhis(4,istru) - qltmin) /dhstru
c
                  if (teken .le. 0.0) then
c                    flow to bekken
                     help = width*qlat(istat,1)**2/(g*area**3)
                     as(istrc) = (1. - help) * dqdhup
                     bs(istrc) = qlat(istat,1)/(g*area**2)*dqdhup
                     cs(istrc) = dqdhdn
                     es(istrc) = -strhis(4,istru)*relstr
                  else
c                    flow from bekken
                     as(istrc) = dqdhdn
                     bs(istrc) = qlat(istat,1)/(g*area**2)*dqdhup
                     help = width*qlat(istat,1)**2/(g*area**3)
                     cs(istrc) = (1. - help) * dqdhup
                     es(istrc) = -strhis(4,istru)*relstr
                  endif
c
               endif
 55         continue
c
c           Correction for compound structures.
c           Multiplication by Q(i) has been carried out already.
c
            do 60 istrc = 1, nstrc

               if (istrc .eq. 1) then
                  istru = is1
               else
                  istru = is2
               endif

               astr = astr + as(istrc)
               bstr = bstr + bs(istrc)
               cstr = cstr + cs(istrc)
               help = as(istrc) * (hstat - 0.5 * (h1(igr) + h1(igr+1)))
c                                  Q_n+1,m         Q_n
     +              + bs(istrc) * (qlat(istat,1) - qlat(istat,3))
c                                  Hr_n+1,m    Hr_n
     +              + cs(istrc) * (hret - qlat(istat,9))
               estr = estr + es(istrc) + help
 60         continue
c
            qlat(istat,4) = astr
            qlat(istat,5) = bstr
            qlat(istat,6) = cstr
            qlat(istat,7) = estr
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
c           istat2         = number of 2nd Qlat station
c           qlat(istat2,1) = Qlat at 2nd station at previous time
c                            level (explicit coupling)
c
            istat2        = int(qltpar(3,istat))
            qlat(istat,1) = -qlat(istat2,1)
        endif
   20 continue
c
      if (lmoza .and. nqlat.gt. 0 ) then
c
c          **********************************************
c          * Qlat from Mozart (iopt=8 , moztyp)         *
c          **********************************************
c
           if(iter .eq. 1 .and. istmoz .eq. 0) then
CJC        if((iter .eq. 1 .and. istmoz .eq. 0) .or.
CJC     +     (iter .eq. 1 .and. istep .eq. 1)) then
c
C JC 12/4/2000: qlat(istat,3) op 0 zetten, wordt gevuld in Readmoq
C             qlat(istat,1) houdt vorige waarde, init = 0
              do istat = 1, nqlat
                iopt = int(qltpar(2,istat))
                if (iopt .eq. moztyp) then
                   if ( qlat(istat,1) .gt. 1.1E+20 ) then
                      qlat(istat,1) = 0.
                   endif
                   qlat(istat,3) = 0.
                endif
              end do
c
c lees q-lat van file
c
c#if !  defined (SHR_MEM)
cc ====  shared memory  ====
cc ====  niet voor SRS BOS
c              Call MOZREADQ (qlat   ,nqlat  ,
c     +                       qlatid ,qltpar ,juer   )
c#endif
          else
c
c JC 12/4/2000: Qlat al gelezen,
c neem opgeslagen waarde (3) en lopende waarde (1) in onderrelaxatie
          do istat = 1, nqlat
            iopt = int(qltpar(2,istat))
            if (iopt .eq. moztyp) then
C JC 12/4/2000: ook in iteratie
              qlat(istat,1) = omqlat*qlat(istat,3) +
     +                       (1.-omqlat)*qlat(istat,1)
            endif
          end do
        endif
      endif
c
c
c      !DEC$ IF DEFINED (_DLL)
c      if (OpenMIactive()) then
c         qlatnm = max(1,gtcpnt ('QLATNM'))
c         ires = GetLaterals(qlat, cp(qlatnm), qltpar, nqlat)
c      endif
c      !DEC$ ENDIF
c
c     *****************************************
c     * Distribution of Qlat over grid points *
c     *****************************************
c
      do 30 igr = 1, ngrid
        qlatgr(igr) = 0.
   30 continue
c
      do 40 istat = 1, nqlat
         call CQLATG (ngrid  ,nqlat  ,istat  ,qlat(istat,1) ,qltpar ,
     +                x      ,qlatgr )
   40 continue
c     write(juer,'(11f10.2)') (qlatgr(igr),igr = 1,110)
c
 1000 continue
      end
