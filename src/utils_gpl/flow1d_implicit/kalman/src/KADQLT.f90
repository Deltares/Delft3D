subroutine KADQLT(g      ,time   ,ngrid  ,x      ,h1     ,h      ,&
&strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,&
&nqlat  ,qltpar ,juer   ,lambda ,theta2 ,dt     ,&
&qlat   ,strclo ,qlatac ,dqltdh ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KADQLT (KAlman Derivative of Q LaTeral)
!
! Module description: Computation of the derivatives of the waterlevel
!                     dependent lateral discharges in the user selected
!                     discharge stations at time level t(n+1/2).
!
!                     Calculate the lateral discharges for an incremented
!                     water level using routine FLQLAT. Use these values
!                     and the lateral discharges obtained in the flow
!                     module to determine the derivatives by numerical
!                     differentiation.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 22 dqltdh(ngrid)     O  Derivative of lateral discharge to waterlevel in
!                         every grid point i+1/2 on time n+1/2
!                         (d(Qlat)/dh). Value at i+1/2 is stored at index
!                         i.
! 18 dt                I  Computational time step dt [sec].
!  1 g                 I  Acceleration of gravity.
!  5 h1(ngrid)         I  Water level in every grid point at time t(n).
!  6 h(ngrid)          I  Contains water levels in every grid point. It is
!                         stored on index 2 of the packed array hpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
! 15 juer              P  -
! 23 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 16 lambda            I  Extra resistance in general structure.
!  9 maxtab            I  Maximum number of defined tables.
!  3 ngrid             I  Number of grid points in network.
! 13 nqlat             I  Number of lateral discharge stations.
! 11 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
! 10 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 19 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
!                         time level n+1/2.
! 21 qlatac(nqlat)     IO Lateral discharge in every lateral discharge
!                         location i on time n+1/2+dh.
! 14 qltpar(9,nqlat)   I  Defines lateral discharge stations (Qlat):
!                         (1,i) = Branch number.
!                         (2,i) = Type of Qlateral definition:
!                                 cqlftm (1) : Qlat = f(t)
!                                 cqlfh  (2) : Qlat = Q(h)
!                                 cqlstr (3) : Qlat from structure
!                                 cqlcon (4) : Qlat from other lateral
!                                              discharge station
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
! 20 strclo            P  -
!  8 strpar            P  -
!  7 strtyp(10,nstru)  I  Structure definitions:
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
! 12 table             P  -
! 17 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  2 time              I  Actual time level tn+1. in sec.
!  4 x                 P  -
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
! $Log: kadqlt.pf,v $
! Revision 1.3  1999/03/15  15:51:39  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:04:45  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:22  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Function declaration
!
   real FLQHSW
   double precision FLQHGS
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer ngrid, maxtab, ntabm, nqlat, juer, ker
   integer strtyp(10,*), ntab(4,maxtab)

   logical strclo(*)

   real    lambda   , theta2
   real    g, qltpar(9,*), qlat(*), qlatac(*), dqltdh(ngrid),&
   &x(ngrid), table(ntabm), strpar(dmstrpar,*)

   double  precision  time, dt, h1(ngrid), h(ngrid)
!
!     Declaration of local variables
!
   character txt*4
   logical   strsta
   integer   igr, iopt, istat, istat2, istru, itab, i1, i2, type
   integer   idum
   real      hup, hdown, hin, hout, lbt, let, hstat, qstat, sign
   real      uu, ud, zs, wstr, cw, slim
   real      dh, dqlat, fred
   double precision w2,   wsd,  zb2, ds1, ds2,   rhoast,&
   &cgf,  cgd,  cwf, cwd, mugf,  dhstru, zsg,&
   &wstrg,dg,   tpth2,    dum
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
   parameter (dh = 0.001)
!
!     ***********************************
!     * Computation of Qlat in stations *
!     ***********************************
!
   do 10 istat = 1, nqlat
      qlatac(istat) = qlat(istat)
!
!        iopt = 2 : Qlat follows from Q(h)-table
!             = 3 : Qlat follows from structure
!             = 4 : Qlat follows from lateral discharge in 2nd lateral
!                   discharge station
!
      iopt = int(qltpar(2,istat))
!
      if (iopt .eq. cqlfh) then
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

         endif

         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &dble(hstat+dh) , qstat      )

         qlatac(istat) = qstat

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
!           time  : t(n+1/2)
!
         istru = int(qltpar(8,istat))
         itab  = int(qltpar(3,istat))
!
         igr   = int(qltpar(5,istat))
         hin   = theta2 * (h(igr) + h(igr+1)) / 2. +&
         &(1. - theta2) * (h1(igr) + h1(igr+1)) / 2. + dh
         tpth2 = dble (theta2 * time + (1. - theta2) * (time - dt))
!
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &tpth2, hout   )
!
!           Determine flow direction, upstream/downstream water level
!           and bottom downstreams the structure
!
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
            call FLSWAR (istru  ,strpar ,sign   ,zs     ,&
            &wstr   ,cw     ,slim   ,itab   )
!
            strsta = .false.
            qlatac(istat) = FLQHSW(g      ,istru  ,strsta ,&
            &strclo ,hup    ,hdown  ,uu     ,&
            &ud     ,zs     ,wstr   ,cw     ,&
            &slim   ,itab   ,maxtab ,ntabm  ,&
            &ntab   ,table  ,fred   ) * sign
         else if ( strtyp(1,istru) .eq. cgenst ) then
!
!              *********************
!              * General Structure *
!              *********************
!              Remark:
!              function FLQHGS incorporates the stage-discharge
!              relation for a general structure
!
            call FLGTAR (istru  ,strpar ,dble(sign)     ,zsg    ,&
            &wstrg  ,w2     ,wsd    ,zb2    ,dg     ,&
            &ds1    ,ds2    ,cgf    ,cgd    ,cwf    ,&
            &cwd    ,mugf   )
!
            strsta = .false.
            rhoast = 1.0D0
            dhstru = 0.0D0
            qlatac(istat) = sngl (FLQHGS (&
            &dble(g)      ,istru ,strsta ,strclo  ,&
            &dble(hup)    ,dble(hdown)   ,dble(uu),&
            &zsg   ,wstrg ,w2    ,wsd    ,zb2     ,&
            &dg    ,ds1   ,ds2   ,rhoast ,cgf     ,&
            &cgd   ,cwf   ,cwd   ,mugf   ,dum     ,&
            &idum  ,dble(lambda) ,dhstru )&
            &) * sign
         else
!
!              ERROR Unexpected type for lateral structure
!
            ker = fatal
            write (txt,'(i4)') istru
            call ERROR (juer,'KADQLT  structure number @'//txt//'@',&
            &eflstr, ker)
            goto 1000
         endif
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
!           istat2       = number of 2nd Qlat station
!           qlat(istat2) = Qlat at 2nd station at previous time
!                         level (explicit coupling)
!
         istat2      = int(qltpar(3,istat))
         qlatac(istat) = -qlatac(istat2)
      endif
20 continue

!
!     *****************************************
!     * Distribution of Qlat over grid points *
!     *****************************************
!
   do 30 igr = 1, ngrid
      dqltdh(igr) = 0.
30 continue
!
   do 40 istat = 1, nqlat
      dqlat = ( qlatac(istat) - qlat(istat) ) / dh
      call CQLATG (ngrid  ,nqlat  ,istat  ,dqlat  ,qltpar ,&
      &x      ,dqltdh )
40 continue
!
1000 continue
end
