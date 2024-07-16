      subroutine saccda (nmouth ,nbran  ,ngrid  ,juer   ,time   ,dt    ,
     &                   tp     ,mouth  ,branch ,bramrl ,q1     ,q2    ,
     &                   grid   ,csa1   ,x      ,dcdx   ,cdcdx0 ,cdcdx1,
     &                   cdcdx2 ,thasca ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SACCDA (SAlt calculation of C/C0*Dc/dx Averaged)
c
c Module description: Calculate the <c/c0*dc/dx> term of the Thatcher-
c                     Harleman formulation.
c                     [ Doc: S-FO-001.5KV / Eq. 19-8 ]
c
c                     For the Thatcher-Harleman and Zwendl dispersion
c                     formulation this term is calculated over the last
c                     tidal period.
c
c                     The calculation starts as soon as one of the
c                     mouths detects the start of a tidal period. This
c                     time is stored as the starting time. During the
c                     tidal period (Tp) time avaraging of the term will
c                     be continued. This term will be kept for 2 tidal
c                     periods.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 bramrl(nmouth+1,  I  Branch-Mouth relation table. The first index
c        ,nbran)          contains the number of related mouths (index
c                         1). The second index contains the first rela-
c                         ted mouth number etc.
c  9 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 17 cdcdx0(ngrid)     IO Contains for every grid point on time t=n+1
c                         the sum of the term c/c0*dc/dx over the cur-
c                         rent tide.
c 18 cdcdx1(ngrid)     IO Contains for every grid point the averaged
c                         value of term <c/c0*dc/dx> over the last tide.
c 19 cdcdx2(ngrid)     O  Contains for every grid point the averaged
c                         value of term <c/c0*dc/dx> over the tide befo-
c                         re the last tide.
c 14 csa1(ngrid)       I  Salt concentration in every grid point at time
c                         t(n).
c 16 dcdx(ngrid)       I  Scratch array for dc/dx.
c  6 dt                I  Computational time step dt [sec].
c 13 grid              P  -
c  4 juer              P  -
c 21 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  8 mouth(2,nmouth)   I  Node numbers which are mouths:
c                         (1,i) = Node number j which is a mouth.
c                         (2,i) = Number of the branch that contains the
c                                 mouth.
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c  1 nmouth            I  Maximum number of mouths in the network.
c 11 q1(ngrid)         I  Discharge in every grid point at time t(n).
c 12 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 20 thasca(3)         IO Administration for the calculation of
c                         <c/c0*dc/dx>:
c                         (1) =   End time of current tide.
c                         (2) =   Number of time steps that contribute
c                                 in current sum.
c                         (3) =   0 : Frst tidal period not started yet.
c                                 1 : Fist tidal period has started.
c  5 time              I  Actual time level tn+1. in sec.
c  7 tp                I  Tidal period   (salt module).
c 15 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c sadcdx  SAlt DC/DX calculation
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: saccda.pf,v $
c Revision 1.7  1999/07/27  15:10:06  kuipe_j
c improve calculation slack
c
c Revision 1.6  1999/06/01  13:42:35  kuipe_j
c names in messages substituted + message template
c
c Revision 1.5  1995/10/18  09:00:15  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/08/30  12:37:13  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:56:04  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:05:54  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:35  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:04  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:25  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nmouth  ,nbran   ,ngrid   ,juer   ,ker
      integer mouth (2,*)      ,branch(4,nbran) ,bramrl(nmouth+1,nbran)
      integer grid  (ngrid)
      real    csa1  (ngrid) ,
     &        x     (ngrid)    ,dcdx  (ngrid)   ,cdcdx0(ngrid) ,
     &        cdcdx1(ngrid)    ,cdcdx2(ngrid)   ,thasca(3    )
      double  precision  time  ,dt    ,tp, q1(ngrid),q2(ngrid)
c
c     Declaration of local parameters
c
      integer   ibr  ,igr  ,i1  ,i2 ,nmj ,im ,i ,j, lbrnam
      real      s    ,frac ,c0jt
      logical   start
      character branam*40
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     Determination of beginning of first tide. All mouths will be
c     checked. If found the summation starts.
c
      im    = 0
      start = int(thasca(3)) .eq. 1
c
c---- do while -- first change of outflow to inflow ---->
   10 continue
      if (.not.start .and. im.lt.nmouth) then
c
c        Determine if mouth is at begin or end of branch.
c
         im  = im+1
         ibr = mouth(2,im)
         if (branch(1,ibr) .eq. mouth(1,im)) then
            igr   = branch(3,ibr)
            s     = 1.
         else
            igr   = branch(4,ibr)
            s     = -1.
         endif
c
c        Change from outflow to inflow (slack water before flood)?
c
         if (q1(igr)*s .le. 0. .and. q2(igr)*s .gt. 0.) then
            start = .true.
            frac  = q2(igr) - q1(igr)
            if (abs(frac*s) .lt. 1.e-10) then
               frac = 0.
            else
               frac = q1(igr) / frac
            endif
            thasca(1) = sngl(time - dt * dble(frac) - dt + tp)
            thasca(3) = 1.
         endif
c
         goto 10
      endif
c <-- end while ---------------------------------------
c
      if (start) then
c
c        End of tidal period. Shift averaged c/c0*dc/dx
c
         if (time .gt. thasca(1) ) then
            do 20 igr =1,ngrid
               cdcdx2(igr) = cdcdx1(igr)
               cdcdx1(igr) = cdcdx0(igr) / thasca(2)
               cdcdx0(igr) = 0.
   20       continue
            thasca(1) = thasca(1) + sngl(tp)
            thasca(2) = 0.
         endif
c
c        Calculate c/c0*dc/dx at time t(n+1). The concentrations of
c        time t(n) are used. At first dc/dx is calculated.
c
         call sadcdx (nbran  ,ngrid  ,branch ,grid   ,csa1   ,x   ,
     &                dcdx   )
c
         do 50 ibr = 1,nbran
c
c           Secondly c0(j,t) , which is constant in a branch,
c           will be calculated.
c
            nmj  = bramrl(1,ibr)
            c0jt = 0.
            do 30 i = 2,nmj+1
               im = bramrl(i,ibr)
               j  = mouth(2,im)
               if (branch(1,j) .eq. mouth(1,im)) then
c
c                 Mouth at begin of branch
                  igr = branch(3,j)
               else
c
c                 Mouth at end of branch
                  igr = branch(4,j)
               endif
               c0jt = c0jt + csa1(igr)
   30       continue
            c0jt = c0jt / nmj
c
c           Check on zero concentrations at mouths
c
            if (c0jt .lt. 1.e-10) then
               ker = fatal
               call getbrn (ibr,branam,lbrnam)
               call error (juer,'SADCDX  branch @'//branam(:lbrnam)//
     &                    '@',esacon,ker)
               goto 1000
            endif
c
c           Calculation per grid point.
c
            i1  = branch(3,ibr)
            i2  = branch(4,ibr)
            do 40 igr = i1,i2
               cdcdx0(igr) = cdcdx0(igr) +
     &                       csa1(igr) / c0jt * abs(dcdx(igr))
   40       continue
c
   50    continue
c
         thasca(2) = thasca(2) + 1.
      endif
c
 1000 continue
c
      end
