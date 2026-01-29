subroutine FLQLAT(g      ,time   ,ngrid  ,lambda ,x      ,h1     ,&
&h      ,strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,&
!                       mozart parameters
&lmoza  ,istmoz ,qlatid ,&
&table  ,nqlat  ,qltpar ,juer   ,qlat   ,qlatgr ,&
&strclo ,strhis ,theta2 ,dt     ,ker    ,omqlat ,&
&dhstru ,relstr ,iter)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLQLAT (FLow Q LATeral)
!
! Module description: In subroutine FLQLAT first the lateral discharges
!                     Qlat will be computed for each station. After this
!                     the calculated values will be distributed over the
!                     points i+1/2.
!
!                     The computation of the lateral discharges can be
!                     split up in the computation of the discharges per
!                     station, followed by the distribution of this
!                     discharge. To one point more than one lateral
!                     discharge may be related. For this reason the
!                     distributed lateral discharges in the points i+1/2
!                     have to be accumulated rather than assigned.
!
!                     The discharge for a station can be defined as:
!
!                     1.  Qlat as time series Qlat (t);
!                     2.  Qlat follows from a Q(h) table;
!                     3.  Qlat follows from structure;
!                     4.  Qlat follows from 2nd lateral discharge sta-
!                         tion;
!                     5.  Qlat defined from water level in retention
!                         area.
!
!                     Options 1, 2 and 5 are to be given as a point
!                     lateral discharge Qlat or on a trajectory as qlat.
!                     Options 3 and 4 are always a point lateral dis-
!                     charge (Qlat).
!
!                     After the computation of the lateral discharge
!                     Qlat in the station, the discharge will be dis-
!                     tributed over the points i+1/2.
!
!                     Point lateral discharge distribution:
!
!                         For a point lateral discharge in grid point i
!                         the discharge is distributed over the points
!                         i-1/2 and i+1/2. (form. 6-4 a/b ,S-FO-
!                         001.5KV).
!
!                         For a point lateral discharge in a grid cell
!                         the lateral discharge for point i+1/2 follows
!                         from formula 6-4 c ,S-FO-001.5KV.
!
!                     Trajectory lateral discharge distribution:
!
!                     For a lateral discharge over a trajectory
!                     [Lbt,Let] there are two separate cases to distin-
!                     guish:
!
!                         - Trajectory within one grid cell
!
!                         Discharge in point i+1/2 follows from formula
!                         6-6 ,S-FO-001.5KV.
!
!                         - Trajectory over more than one grid cell
!
!                         Several formulas are used here:
!
!                         -   Begin of trajectory (part of grid cell
!                             enclosed) : formula 6-5 a ,S-FO-001.5KV .
!                         -   End of trajectory (part of grid cell
!                             enclosed) : formula 6-5 b ,S-FO-001.5KV .
!                         -   Enclosed grid cells: lateral discharge
!                             equals qlat
!
!                     Structures:
!
!                     One of the possibilities to define a lateral dis-
!                     charge is to use a structure. Only the simple weir
!                     and simple gate can be used. To calculate the lat-
!                     eral discharge the QH relation routines for these
!                     structures will be called. These subroutines need
!                     two water levels. One of the water levels follow
!                     from the grid point where the lateral discharge
!                     has been defined. The other water follows from a
!                     user given table (hout(side)).
!
!                     The simple weir subroutine also needs a velocity
!                     to calculate the energy level. The velocity how-
!                     ever is unknown and is therefore set to zero. The
!                     formulas will operate with water levels in stead
!                     of energy levels.
!
!                     Q(h) table
!
!                     The possibility for a Q(h) table can be point
!                     lateral or on a trajectory. In case a trajectory
!                     has been specified an averaged water level or
!                     depth over the trajectory is calculated in the
!                     routine FLHAVG.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 22 dt                I  Computational time step dt [sec].
!  1 g                 I  Acceleration of gravity.
!  6 h1(ngrid)         I  Water level in every grid point at time t(n).
!  7 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!    iter              I  Iteration step.
! 16 juer              P  -
! 23 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  4 lambda            I  Extra resistance in general structure.
! 10 maxtab            I  Maximum number of defined tables.
!  3 ngrid             I  Number of grid points in network.
! 14 nqlat             I  Number of lateral discharge stations.
! 12 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
! 11 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 24 omqlat            I  underrelaxation parameter omega for lateral
!                         discharges
! 17 qlat(nqlat)       IO (i) = Actual lateral discharge in station i on
!                         time level n+1/2.
! 18 qlatgr(ngrid)     O  (i) = Actual lateral discharge in grid point
!                         i+1/2.
! 15 qltpar(9,nqlat)   I  Defines lateral discharge stations (Qlat):
!                         (1,i) = Branch number.
!                         (2,i) = Type of Qlateral definition:
!                                 cqlftm (1) : Qlat = f(t)
!                                 cqlfh  (2) : Qlat = Q(h)
!                                 cqlstr (3) : Qlat from structure
!                                 cqlcon (4) : Qlat from other lateral
!                                              discharge station
!                                 cqlret (5) : Qlat from retention area
!                         - For types 1 and 2 (functns of time or Q(h)):
!                         (3,i) = Table number.
!                         (4,i) = Point or traject type:
!                                 cpd1gp (1) : Point discharge 1 gpoint
!                                 cpd1gc (2) : Point discharge 1 cell
!                                 ctd1gc (3) : Traject discharge 1 cell
!                                 ctdmgc (4) : Traject discharge over
!                                              more grid cells
!                         (5,i) = First gridpoint of cell/trajectory
!                                 (types 1,2,3,4).
!                         (6,i) = Last gridpoint of cell/trajectory
!                                 (types 2,3,4).
!                         (7,i) = Lb coordinate for cell/trajectory
!                                 (types 3,4).
!                         (8,i) = Le coordinate for cell/trajectory
!                                 (types 3,4).
!                         - For type 3 (structure):
!                         (3,i) = Table number of outside water level
!                                 table.
!                         (4,i) = Point or traject type:
!                                 cpd1gp (1) : Point discharge 1 gpoint
!                                 cpd1gc (2) : Point discharge 1 cell
!                         (5,i) = First gridpoint of cell.
!                         (6,i) = Last gridpoint of cell.
!                         (7,i) = Structure type.
!                         (8,i) = Structure number.
!                         - For type 4 (connection point):
!                         (3,i) = Second index of qltpar which is the
!                                 connection point.
!                         (4,i) = Point or traject type:
!                                 cpd1gp (1) : Point discharge 1 gpoint
!                                 cpd1gc (2) : Point discharge 1 cell
!                         (5,i) = First gridpoint of cell.
!                         (6,i) = Last gridpoint of cell.
!                         Below a drawing is given which defines the
!                         locations of the grid points and trajectory
!                         (Lb , Le):
!                            x=Lb                    x=Le
!                             |                       |
!                         -+-------+--------------+--------+-----
!                         i1  |   i1+1            i2  |   i2+1
!                         - For type 5 (retention area):
!                         (3,i) = Retention area in m^2
!                         (4,i) = Point or traject type:
!                                 cpd1gp (1) : Point discharge 1 gpoint
!                                 cpd1gc (2) : Point discharge 1 cell
!                         (5,i) = First gridpoint of cell.
!                         (6,i) = Last gridpoint of cell.
!                         (7,i) = Bed level in retention area
!                         (8,i) = Water level in retention area at start
!                                 of timestep
!                         (9,i) = Structure number, ist, where:
!                                 ist = is1 + 1000*is2
!                                 is1 = first structure
!                                 is2 = second structure (optional)
! 19 strclo            P  -
! 20 strhis(13,nstru)  O  For each structure the discharge and the
!                         parameters to be controlled must be saved to
!                         be able to write to the output file. This will
!                         be done in array strhis(8,nstru). This array
!                         will also be used to check the values of the
!                         controlled parameters or to determine if
!                         increase(open) or decrease(close) of these
!                         parameters occurs. This array will also be
!                         part of the restart file.
!                         (1,i) = Gate height
!                         (2,i) = Crest height
!                         (3,i) = Crest width
!                         (4,i) = Discharge through structure
!                         (5,i) = Gate height at previous time step
!                         (6,i) = Crest height at previous time step
!                         (7,i) = Crest width at previous time step
!                         (8,i) = Flow condition of general structure:
!                                 formno = 0, closed or other structure
!                                 formno = 1, free weir
!                                 formno = 2, drowned weir
!                                 formno = 3, free gate
!                                 formno = 4, drowned gate
!                         (9,i) = coefficient Q-H-realtion asde
!                         (10,i)= coefficient Q-H-realtion bsde
!                         (11,i)= coefficient Q-H-realtion csde
!                         (12,i)= coefficient Q-H-realtion dsde
!                         (13,i)= water level in retention area
!  9 strpar            P  -
!  8 strtyp(10,nstru)  I  Structure definitions:
!                         (1,i) = Type of structure:
!                                 csweir (1) : Simple weir
!                                 caweir (2) : Advanced weir
!                                 csgate (3) : - not used, reserved for
!                                                simple gate -
!                                 cpump  (4) : Pump
!                                 cgenst (5) : General Structure
!                         (2,i) = Position of structure:
!                                 cstbra (1) : In branch
!                                 cstlat (2) : Lateral structure
!                         (3,i) = Left gridpoint.
!                         (4,i) = Right gridpoint.
!                         (5,i) = dummy
!                         (6,i) = dummy
!                         (7,i) = dummy
!                         (8,i) = dummy
!                         (9,i) = dummy
!                         (10,i)= dummy
! 13 table             P  -
! 21 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  2 time              I  Actual time level tn+1. in sec.
!  5 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! cqlatg  Calculate Q LaTeral per Grid point
! error   write an ERROR to the error file.
! flgtar  FLow get General sTructure ARguments
! flhavg  FLow AVeraGed waterlevel H
! flswar  FLow get Simple Weir ARguments
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flqlat.pf,v $
! Revision 1.13  1999/06/01  13:42:20  kuipe_j
! names in messages substituted + message template
!
! Revision 1.12  1999/03/15  14:24:42  kuipe_j
! Bug fix lateral discharge on t=0
!
! Revision 1.11  1997/11/04  14:17:28  kuipe_j
! Retention basin
!
! Revision 1.10  1997/01/23  08:29:16  kuipe_j
! Make flow module robust
!
! Revision 1.9  1996/04/12  13:04:19  kuipe_j
! headers, minor changes
!
! Revision 1.8  1996/04/11  08:23:52  kuipe_j
! Kalman module added
!
! Revision 1.7  1996/01/17  14:38:45  kuipe_j
! header update
!
! Revision 1.6  1995/09/22  10:02:10  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/09/12  08:11:01  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1995/08/30  12:36:50  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:24  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:24  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:04  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:50  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:28  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:54  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!      !DEC$ IF DEFINED (_DLL)
!      use SobekRE_OpenMI
!      !DEC$ ENDIF
!
!     Function declaration
!
   real             FLQHSW
   double precision FLQHGS
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
   include '../include/mempool.i'
!
!     Declaration of parameters
!
   integer ngrid, maxtab, ntabm, nqlat, juer, ker
   integer strtyp(10,*), ntab(4,maxtab), iter
!     Koppeling Mozart
   integer istmoz
   logical lmoza
   character(len=40) qlatid(*)
!
!
   logical strclo(*)
   real    lambda   , theta2 , omqlat, dhstru, relstr
   real    g, qltpar(9,*), qlat(nqlat,9), qlatgr(ngrid),&
   &x(ngrid), table(ntabm),&
   &strpar(dmstrpar,*), strhis(dmstrh,*)
   double  precision  time
   double  precision  dt, dhsdub
   double  precision  h1(ngrid), h(ngrid)
!
!     Declaration of local variables
!
   logical   strsta
   integer   igr, iopt, istat, istat2, istru, itab, i1, i2, type
   integer   formno
   integer   is1, is2, istrc, nstrc
   real      hup, hdown, hin,  hout, lbt, let, hstat, qstat,  teken
   real      uu,  ud,    zs,   wstr, cw,  slim,dum1 , qltstr
   real      hret
   real      astr, bstr, cstr, estr, area, area1, width
   real      as(2), bs(2), cs(2), es(2), help
   real      w1, zb1, qltpls, qltmin, dqdhup, dqdhdn
   double precision      w2,   wsd,  zb2, ds1, ds2,   rhoast,&
   &cgf,  cgd,  cwf, cwd, mugf,  zsg,&
   &wstrg,dg,   tpth2,    dum2
!
!     Koppeling Mozart
   integer  , parameter ::  moztyp=8
!
!     Include sobek error code file
!
   include '../include/errcod.i'
   include '../include/sobcon.i'
!
!     ***********************************
!     * Computation of Qlat in stations *
!     ***********************************
!
   dhsdub = dhstru
   do 10 istat = 1, nqlat
!
!        iopt = 1 : Qlat defined as time series
!             = 2 : Qlat follows from Q(h)-table
!             = 3 : Qlat follows from structure
!             = 4 : Qlat follows from lateral discharge in 2nd lateral
!                   discharge station
!             = 5 : Qlat determined from water level in retention
!                   area
!
      iopt = int(qltpar(2,istat))
!
! JC 12/4/2000: oorspr. Mozart aanpassingen verwijderd uit onderstaande
      if (iopt .eq. cqlftm) then
!
!           **********************************************
!           * Qlat defined as a function of time (iopt=1)*
!           **********************************************
!
!           itab : TABLE number table Qlat=f(t)
!c          time : t(n+1/2)
!           time : t(n+1)
!
         itab  = int(qltpar(3,istat))
         tpth2 = dble (theta2 * time + (1. - theta2) * (time - dt))

         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &tpth2,qstat       )
!
!           Lataral discharge with underrelaxation
!
         if ( qlat(istat,1) .gt. 1.1E+20 ) then
            qlat(istat,1) = qstat
         else
            qlat(istat,1) = omqlat*qstat+(1.-omqlat)*qlat(istat,1)
         endif

      else if (iopt .eq. cqlfh) then
!
!           **********************************************
!           * Qlat from Q(h)-table              (iopt=2) *
!           **********************************************
!
!           itab  : TABLE number for Q(h)-table
!           hstat : water level h in Qlat station
!
         itab  = int(qltpar(3,istat))
         type  = int(qltpar(4,istat))
!
!           type:
!           1. point discharge in one gridpoint
!           2. point discharge in grid cell
!           3. traject discharge in grid cell
!           4. traject discharge over more grid cells
!
         if (type .eq. cpd1gp) then

            igr   = int(qltpar(5,istat))
            hstat = theta2 * h(igr) + (1. - theta2) * h1(igr)

         else if (type .eq. cpd1gc) then

            igr   = int(qltpar(5,istat))
            hstat = theta2 * (h(igr) + h(igr+1)) / 2. +&
            &(1. - theta2) * (h1(igr) + h1(igr+1)) / 2.

         else if (type .eq. ctd1gc .or. type .eq. ctdmgc) then

            i1  = int(qltpar(5,istat))
            i2  = int(qltpar(6,istat))
            lbt = qltpar(7,istat)
            let = qltpar(8,istat)
            call FLHAVG (ngrid, x, h1, h, lbt, let, i1, i2, hstat,&
            &theta2)
!
         endif

         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &dble(hstat) , qstat      )
!
!           Lataral discharge with underrelaxation
!
         if ( qlat(istat,1) .gt. 1.1E+20 ) then
            qlat(istat,1) = qstat
         else
            qlat(istat,1) = omqlat*qstat+(1.-omqlat)*qlat(istat,1)
         endif

      else if (iopt .eq. cqlfst) then
!
!           **********************************************
!           * Qlat from structure               (iopt=3) *
!           **********************************************
!
!           istru : number of structure in Qlat station
!           hstat : water level h in nearest gridpoint
!           hout  : outside water level
!           itab  : TABLE number table hout=f(t)
!c          time  : t(n+1/2)
!           time  : t(n+1)
!
         istru   = int(qltpar(8,istat))
         itab    = int(qltpar(3,istat))
         type    = int(qltpar(4,istat))
!
         if (type .eq. cpd1gp) then

            igr   = int(qltpar(5,istat))
            hin   = theta2 * h(igr) + (1. - theta2) * h1(igr)

         else if (type .eq. cpd1gc) then

            igr   = int(qltpar(5,istat))
            hin   = theta2 * (h(igr) + h(igr+1)) / 2. +&
            &(1. - theta2) * (h1(igr) + h1(igr+1)) / 2.

         endif
         tpth2 = dble (theta2 * time + (1. - theta2) * (time - dt))

         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
!    +                   time, hout   )
         &tpth2,hout   )
!
!           Determine flow direction, upstream/downstream water level
!           and bottom downstreams the structure
!
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
!
         if ( strtyp(1,istru) .eq. csweir ) then
!
!              ***************
!              * Simple weir *
!              ***************
!              Remark:
!              function FLQHSW incorporates the stage-discharge
!              relation for a simple weir
!
            call FLSWAR (istru  ,strpar  ,teken   ,zs     ,&
            &wstr   ,cw     ,slim   ,itab   )
!
            strsta = .true.
            qltstr = FLQHSW(g      ,istru  ,strsta ,&
            &strclo ,hup    ,hdown  ,uu     ,&
            &ud     ,zs     ,wstr   ,cw     ,&
            &slim   ,itab   ,maxtab ,ntabm  ,&
            &ntab   ,table  ,dum1 ) *  teken
!
!           Lataral discharge with underrelaxation
!

            if ( qlat(istat,1) .gt. 1.1E+20 ) then
               qlat(istat,1) = qltstr
            else
               qlat(istat,1) = omqlat*qltstr&
               &+ (1.-omqlat)*qlat(istat,1)
            endif
!
            strhis(4,istru) = qlat(istat,1)
            strsta = .false.
         else if ( strtyp(1,istru) .eq. cgenst ) then
!
!              *********************
!              * General Structure *
!              *********************
!              Remark:
!              function FLQHGS incorporates the stage-discharge
!              relation for a general structure
!
            call FLGTAR (istru  ,strpar ,dble(teken)    ,zsg    ,&
            &wstrg  ,w2     ,wsd    ,zb2    ,dg     ,&
            &ds1    ,ds2    ,cgf    ,cgd    ,cwf    ,&
            &cwd    ,mugf   )
!
            strsta = .true.
            rhoast = 1.0D0
            qltstr = real(FLQHGS (dble(g),istru  ,strsta ,&
            &strclo         ,dble(hup)      ,&
            &dble(hdown)    ,dble(uu)       ,&
            &zsg    ,wstrg  ,w2     ,wsd    ,&
            &zb2    ,dg     ,ds1    ,ds2    ,&
            &rhoast ,cgf    ,cgd    ,cwf    ,&
            &cwd    ,mugf   ,dum2   ,formno ,&
            &dble(lambda)   ,dhsdub )&
            &* teken, kind=kind(qltstr))
!
!           Lataral discharge with underrelaxation
!
            if ( qlat(istat,1) .gt. 1.1E+20 ) then
               qlat(istat,1) = qltstr
            else
               qlat(istat,1) = omqlat*qltstr&
               &+ (1.-omqlat)*qlat(istat,1)
            endif
!
            strhis(8,istru) = formno
            strhis(4,istru) = qlat(istat,1)
            strsta = .false.
         else
!
!              ERROR Unexpected type for lateral structure
!
            ker = fatal
!              call getstr(istru,strnam,lstnam)
!              call sre_error (juer,'FLQLAT  structure number@'//
!     +        strnam(:lstnam)//'@',eflstr, ker)
            goto 1000
         endif
      else if ( iopt .eq. cqlret ) then
!
!
!
!           **************************************************
!           * Qlat from retention area (iopt = 5)            *
!           **************************************************
!
!           determine structure ids and discharge type
!
         is1 = MOD(INT(qltpar(9,istat)), 1000)
         is2 = INT(qltpar(9,istat))/1000
         type = INT(qltpar(4,istat))
!
!           Set water level and discharge for the first iteration step
!           to the values of the previous time step N+1
!
         if (iter .eq. 1) then
!             Hr_n+1,m          Hr_N+1
            qltpar(8,istat) = strhis(13,is1)
!             Hr_n              Hr_N+1
            qlat(istat,9)   = strhis(13,is1)
            if ( qlat(istat,1) .gt. 1.1E+20 ) qlat(istat,1) = 0.
!             Qr_n              Qr_N+1
            qlat(istat,3)   = qlat(istat,1)
!             Qr_n+1,m-1        Qr_N+1
            qlat(istat,2)   = qlat(istat,1)
         endif

!
!           determine water level difference
!           ### this should be replaced later as qlat mst be in a
!               cell and not on a grid point ###
!
         if (type .eq. cpd1gc) then
            igr   = int(qltpar(5,istat))
!               hstat = theta2 * (h(igr) + h(igr+1)) / 2. +
!     +                 (1. - theta2) * (h1(igr) + h1(igr+1)) / 2.
            hstat = (h(igr) + h(igr+1)) / 2.
         else
!
!             ERROR Retention areas cannot be defined on grid points
!
            ker = fatal
!              call getstr(is1,strnam,lstnam)
!              call sre_error (juer,'FLQLAT structure@'//strnam(:lstnam)//
!     +                   '@ must be in a grid cell', eflqlt, ker)
            goto 1000
         endif
!                  Hr_n+1,m
         hret = qltpar(8,istat)
!
         if ( hret .gt. hstat ) then
            hup   = hret
            hdown = hstat
            teken = 1.0
         else
            hup   = hstat
            hdown = hret
            teken = -1.0
         endif
!
!           initialise structure coefficients
!           a, c and e are simply added for each structure
!           b uses a weighting. There is no d term.
!
         astr  = 0.0
         bstr  = 0.0
         cstr  = 0.0
         estr  = 0.0
!
!           loop over one or two structures
!
         nstrc = 1
         if (is2 .ne. 0) nstrc = 2

         do 50 istrc = 1, nstrc

            if (istrc .eq. 1) then
               istru = is1
            else
               istru = is2
            endif
!
            if ( strtyp(1,istru) .eq. cgenst ) then
!
!                 *********************
!                 * General Structure *
!                 *********************
!

!                 Remark: It is assumed that one structure
!                 is for positive flow and one for negative flow
!                 So the areas and widths should not be added.
!
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
!
            else
!
!                 ERROR Unexpected type for lateral structure
!
               ker = fatal
!                  call getstr(istru,strnam,lstnam)
!                  call sre_error (juer,
!     +                       'FLQLAT  structure number @'//
!     +                        strnam(:lstnam)//'@',
!     +                        eflstr, ker)
               goto 1000
            endif
50       continue
!
!            Water level of retention area is assumed to be
!            equal to upstream energy height in case of flow
!            from retention area
!
         area = max (area,.1)
         if (teken .gt. 0.0) then
            uu = 0.0
         else
            uu   = qlat(istat,1) / area
!               uu = 0.0
         endif
!
         do 55 istrc = 1, nstrc

            if (istrc .eq. 1) then
               istru = is1
            else
               istru = is2
            endif
!
            if ( strtyp(1,istru) .eq. cgenst ) then
!
!                 *********************
!                 * General Structure *
!                 *********************
!                 Remark:
!                 function FLQHGS incorporates the stage-discharge
!                 relation for a general structure
!
               if (teken .gt. 0.0) then
                  w1  = strpar( 1, istru)
               else
                  w1  = strpar( 9, istru)
               endif
!
               call FLGTAR (istru  ,strpar ,dble(teken)    ,zsg    ,&
               &wstrg  ,w2     ,wsd    ,zb2    ,dg     ,&
               &ds1    ,ds2    ,cgf    ,cgd    ,cwf    ,&
               &cwd    ,mugf   )
!
               strsta = .true.
               rhoast = 1.0D0
               qltstr = real(FLQHGS (dble(g),istru  ,strsta ,&
               &strclo         ,dble(hup)      ,&
               &dble(hdown)    ,dble(uu)       ,&
               &zsg    ,wstrg  ,w2     ,wsd    ,&
               &zb2    ,dg     ,ds1    ,ds2    ,&
               &rhoast ,cgf    ,cgd    ,cwf    ,&
               &cwd    ,mugf   ,dum2   ,formno ,&
               &dble(lambda)   ,dhsdub )&
               &* teken, kind=kind(qltstr))
!
               strhis(8,istru) = formno
               strhis(4,istru) = qltstr
               strsta = .false.
!
               qltstr = real(FLQHGS (dble(g),istru  ,strsta ,&
               &strclo         ,dble(hup)+dhsdub,&
               &dble(hdown)    ,dble(uu)       ,&
               &zsg    ,wstrg  ,w2     ,wsd    ,&
               &zb2    ,dg     ,ds1    ,ds2    ,&
               &rhoast ,cgf    ,cgd    ,cwf    ,&
               &cwd    ,mugf   ,dum2   ,formno ,&
               &dble(lambda)   ,dhsdub )&
               &* teken, kind=kind(qltstr))
               qltpls = qltstr
               dqdhup = (qltpls - strhis(4,istru)) / dhstru
!
               qltstr = real(FLQHGS (dble(g),istru  ,strsta ,&
               &strclo         ,dble(hup)      ,&
               &dble(hdown)-dhsdub,dble(uu)       ,&
               &zsg    ,wstrg  ,w2     ,wsd    ,&
               &zb2    ,dg     ,ds1    ,ds2    ,&
               &rhoast ,cgf    ,cgd    ,cwf    ,&
               &cwd    ,mugf   ,dum2   ,formno ,&
               &dble(lambda)   ,dhsdub )&
               &* teken, kind=kind(qltstr))
               qltmin = qltstr
               dqdhdn = (strhis(4,istru) - qltmin) /dhstru
!
               if (teken .le. 0.0) then
!                    flow to bekken
                  help = width*qlat(istat,1)**2/(g*area**3)
                  as(istrc) = (1. - help) * dqdhup
                  bs(istrc) = qlat(istat,1)/(g*area**2)*dqdhup
                  cs(istrc) = dqdhdn
                  es(istrc) = -strhis(4,istru)*relstr
               else
!                    flow from bekken
                  as(istrc) = dqdhdn
                  bs(istrc) = qlat(istat,1)/(g*area**2)*dqdhup
                  help = width*qlat(istat,1)**2/(g*area**3)
                  cs(istrc) = (1. - help) * dqdhup
                  es(istrc) = -strhis(4,istru)*relstr
               endif
!
            endif
55       continue
!
!           Correction for compound structures.
!           Multiplication by Q(i) has been carried out already.
!
         do 60 istrc = 1, nstrc

            if (istrc .eq. 1) then
               istru = is1
            else
               istru = is2
            endif

            astr = astr + as(istrc)
            bstr = bstr + bs(istrc)
            cstr = cstr + cs(istrc)
            help = as(istrc) * (hstat - 0.5 * (h1(igr) + h1(igr+1)))&
!                                  Q_n+1,m         Q_n
            &+ bs(istrc) * (qlat(istat,1) - qlat(istat,3))&
!                                  Hr_n+1,m    Hr_n
            &+ cs(istrc) * (hret - qlat(istat,9))
            estr = estr + es(istrc) + help
60       continue
!
         qlat(istat,4) = astr
         qlat(istat,5) = bstr
         qlat(istat,6) = cstr
         qlat(istat,7) = estr
      endif
10 continue
!
   do 20 istat = 1, nqlat
!
      iopt = int(qltpar(2,istat))
!
      if (iopt .eq. cqlcon) then
!
!           **********************************************
!           * Qlat from other discharge station (iopt=4) *
!           **********************************************
!
!           istat2         = number of 2nd Qlat station
!           qlat(istat2,1) = Qlat at 2nd station at previous time
!                            level (explicit coupling)
!
         istat2        = int(qltpar(3,istat))
         qlat(istat,1) = -qlat(istat2,1)
      endif
20 continue
!
   if (lmoza .and. nqlat.gt. 0 ) then
!
!          **********************************************
!          * Qlat from Mozart (iopt=8 , moztyp)         *
!          **********************************************
!
      if(iter .eq. 1 .and. istmoz .eq. 0) then
!JC        if((iter .eq. 1 .and. istmoz .eq. 0) .or.
!JC     +     (iter .eq. 1 .and. istep .eq. 1)) then
!
! JC 12/4/2000: qlat(istat,3) op 0 zetten, wordt gevuld in Readmoq
!             qlat(istat,1) houdt vorige waarde, init = 0
         do istat = 1, nqlat
            iopt = int(qltpar(2,istat))
            if (iopt .eq. moztyp) then
               if ( qlat(istat,1) .gt. 1.1E+20 ) then
                  qlat(istat,1) = 0.
               endif
               qlat(istat,3) = 0.
            endif
         end do
!
! lees q-lat van file
!
!#if !  defined (SHR_MEM)
!c ====  shared memory  ====
!c ====  niet voor SRS BOS
!              Call MOZREADQ (qlat   ,nqlat  ,
!     +                       qlatid ,qltpar ,juer   )
!#endif
      else
!
! JC 12/4/2000: Qlat al gelezen,
! neem opgeslagen waarde (3) en lopende waarde (1) in onderrelaxatie
         do istat = 1, nqlat
            iopt = int(qltpar(2,istat))
            if (iopt .eq. moztyp) then
! JC 12/4/2000: ook in iteratie
               qlat(istat,1) = omqlat*qlat(istat,3) +&
               &(1.-omqlat)*qlat(istat,1)
            endif
         end do
      endif
   endif
!
!
!      !DEC$ IF DEFINED (_DLL)
!      if (OpenMIactive()) then
!         qlatnm = max(1,gtcpnt ('QLATNM'))
!         ires = GetLaterals(qlat, cp(qlatnm), qltpar, nqlat)
!      endif
!      !DEC$ ENDIF
!
!     *****************************************
!     * Distribution of Qlat over grid points *
!     *****************************************
!
   do 30 igr = 1, ngrid
      qlatgr(igr) = 0.
30 continue
!
   do 40 istat = 1, nqlat
      call CQLATG (ngrid  ,nqlat  ,istat  ,qlat(istat,1) ,qltpar ,&
      &x      ,qlatgr )
40 continue
!     write(juer,'(11f10.2)') (qlatgr(igr),igr = 1,110)
!
1000 continue
end
