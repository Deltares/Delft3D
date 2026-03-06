subroutine saccda (nmouth ,nbran  ,ngrid  ,juer   ,time   ,dt    ,&
&tp     ,mouth  ,branch ,bramrl ,q1     ,q2    ,&
&grid   ,csa1   ,x      ,dcdx   ,cdcdx0 ,cdcdx1,&
&cdcdx2 ,thasca ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SACCDA (SAlt calculation of C/C0*Dc/dx Averaged)
!
! Module description: Calculate the <c/c0*dc/dx> term of the Thatcher-
!                     Harleman formulation.
!                     [ Doc: S-FO-001.5KV / Eq. 19-8 ]
!
!                     For the Thatcher-Harleman and Zwendl dispersion
!                     formulation this term is calculated over the last
!                     tidal period.
!
!                     The calculation starts as soon as one of the
!                     mouths detects the start of a tidal period. This
!                     time is stored as the starting time. During the
!                     tidal period (Tp) time avaraging of the term will
!                     be continued. This term will be kept for 2 tidal
!                     periods.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 bramrl(nmouth+1,  I  Branch-Mouth relation table. The first index
!        ,nbran)          contains the number of related mouths (index
!                         1). The second index contains the first rela-
!                         ted mouth number etc.
!  9 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 17 cdcdx0(ngrid)     IO Contains for every grid point on time t=n+1
!                         the sum of the term c/c0*dc/dx over the cur-
!                         rent tide.
! 18 cdcdx1(ngrid)     IO Contains for every grid point the averaged
!                         value of term <c/c0*dc/dx> over the last tide.
! 19 cdcdx2(ngrid)     O  Contains for every grid point the averaged
!                         value of term <c/c0*dc/dx> over the tide befo-
!                         re the last tide.
! 14 csa1(ngrid)       I  Salt concentration in every grid point at time
!                         t(n).
! 16 dcdx(ngrid)       I  Scratch array for dc/dx.
!  6 dt                I  Computational time step dt [sec].
! 13 grid              P  -
!  4 juer              P  -
! 21 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  8 mouth(2,nmouth)   I  Node numbers which are mouths:
!                         (1,i) = Node number j which is a mouth.
!                         (2,i) = Number of the branch that contains the
!                                 mouth.
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
!  1 nmouth            I  Maximum number of mouths in the network.
! 11 q1(ngrid)         I  Discharge in every grid point at time t(n).
! 12 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 20 thasca(3)         IO Administration for the calculation of
!                         <c/c0*dc/dx>:
!                         (1) =   End time of current tide.
!                         (2) =   Number of time steps that contribute
!                                 in current sum.
!                         (3) =   0 : Frst tidal period not started yet.
!                                 1 : Fist tidal period has started.
!  5 time              I  Actual time level tn+1. in sec.
!  7 tp                I  Tidal period   (salt module).
! 15 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! sadcdx  SAlt DC/DX calculation
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: saccda.pf,v $
! Revision 1.7  1999/07/27  15:10:06  kuipe_j
! improve calculation slack
!
! Revision 1.6  1999/06/01  13:42:35  kuipe_j
! names in messages substituted + message template
!
! Revision 1.5  1995/10/18  09:00:15  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.4  1995/08/30  12:37:13  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:56:04  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:05:54  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:35  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:04  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:25  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nmouth  ,nbran   ,ngrid   ,juer   ,ker
   integer mouth (2,*)      ,branch(4,nbran) ,bramrl(nmouth+1,nbran)
   integer grid  (ngrid)
   real    csa1  (ngrid) ,&
   &x     (ngrid)    ,dcdx  (ngrid)   ,cdcdx0(ngrid) ,&
   &cdcdx1(ngrid)    ,cdcdx2(ngrid)   ,thasca(3    )
   double  precision  time  ,dt    ,tp, q1(ngrid),q2(ngrid)
!
!     Declaration of local parameters
!
   integer   ibr  ,igr  ,i1  ,i2 ,nmj ,im ,i ,j, lbrnam
   real      s    ,frac ,c0jt
   logical   start
   character branam*40
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     Determination of beginning of first tide. All mouths will be
!     checked. If found the summation starts.
!
   im    = 0
   start = int(thasca(3)) .eq. 1
!
!---- do while -- first change of outflow to inflow ---->
10 continue
   if (.not.start .and. im.lt.nmouth) then
!
!        Determine if mouth is at begin or end of branch.
!
      im  = im+1
      ibr = mouth(2,im)
      if (branch(1,ibr) .eq. mouth(1,im)) then
         igr   = branch(3,ibr)
         s     = 1.
      else
         igr   = branch(4,ibr)
         s     = -1.
      endif
!
!        Change from outflow to inflow (slack water before flood)?
!
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
!
      goto 10
   endif
! <-- end while ---------------------------------------
!
   if (start) then
!
!        End of tidal period. Shift averaged c/c0*dc/dx
!
      if (time .gt. thasca(1) ) then
         do 20 igr =1,ngrid
            cdcdx2(igr) = cdcdx1(igr)
            cdcdx1(igr) = cdcdx0(igr) / thasca(2)
            cdcdx0(igr) = 0.
20       continue
         thasca(1) = thasca(1) + sngl(tp)
         thasca(2) = 0.
      endif
!
!        Calculate c/c0*dc/dx at time t(n+1). The concentrations of
!        time t(n) are used. At first dc/dx is calculated.
!
      call sadcdx (nbran  ,ngrid  ,branch ,grid   ,csa1   ,x   ,&
      &dcdx   )
!
      do 50 ibr = 1,nbran
!
!           Secondly c0(j,t) , which is constant in a branch,
!           will be calculated.
!
         nmj  = bramrl(1,ibr)
         c0jt = 0.
         do 30 i = 2,nmj+1
            im = bramrl(i,ibr)
            j  = mouth(2,im)
            if (branch(1,j) .eq. mouth(1,im)) then
!
!                 Mouth at begin of branch
               igr = branch(3,j)
            else
!
!                 Mouth at end of branch
               igr = branch(4,j)
            endif
            c0jt = c0jt + csa1(igr)
30       continue
         c0jt = c0jt / nmj
!
!           Check on zero concentrations at mouths
!
         if (c0jt .lt. 1.e-10) then
            ker = fatal
            call getbrn (ibr,branam,lbrnam)
            call error (juer,'SADCDX  branch @'//branam(:lbrnam)//&
            &'@',esacon,ker)
            goto 1000
         endif
!
!           Calculation per grid point.
!
         i1  = branch(3,ibr)
         i2  = branch(4,ibr)
         do 40 igr = i1,i2
            cdcdx0(igr) = cdcdx0(igr) +&
            &csa1(igr) / c0jt * abs(dcdx(igr))
40       continue
!
50    continue
!
      thasca(2) = thasca(2) + 1.
   endif
!
1000 continue
!
end
