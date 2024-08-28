      subroutine IVBDSM(time   ,dt     ,istep  ,nstep  ,first ,wrirst ,
     +                  ngrid  ,nbran  ,branch ,x      ,qp    ,hp     ,
     +                  hmax   ,gridnm )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Kuipers
c
c Module:             IVBDSM (special feature for IVBDoS, Main routine)
c
c Module description:
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 time              I  Actual time level (at t=n+1) in sec.
c  2 dt                I  Previous time level (at t=n) in sec.
c  3 istep             I  Current time step number (t(n+1)).
c  4 nstep             I  Last time step number.
c  5 first             I  True in case of first call.
c  6 wrirst            I  True when restart info must be written.
c  7 ngrid             I  Number of grid points in network.
c  8 nbran             I  Number of branches.
c  9 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 10 x(ngrid)          I  Coordinates in every grid point.
c 11 qp(ngrid)         I  Discharge in every grid point
c 12 h(ngrid)          I  Water level in every grid point at t=n+1
c                         (TIME).
c 13 gridnm(ngrid)     I  Name of every grid point.
c 14 hmax(ngrid)      IO  Maximum water level in every grid point.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c IVBOUT  IVBdos OUTput of maximum water levels
c
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: ivbdsm.pf,v $
c Revision 1.1  1999/03/15  14:30:14  kuipe_j
c IVBDOS
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer          istep ,nstep    ,ngrid      ,nbran
      integer          branch(4,nbran)
      real             x     (ngrid)   ,hmax(ngrid)
      double precision time  ,dt, qp(ngrid,3),hp(ngrid,3)
      logical          first ,wrirst
      character(len=40) gridnm(ngrid)

      logical          ivbact,maxwl,restrt
      common  /ivbdos/ ivbact,maxwl,restrt
c
c     Declaration of local variables:
c
      integer          kode
      double precision timep

c    Set flags false. These flags will be overruled if
c    there is a IVBDOS.INI file

      if (first) then
          maxwl  = .false.
          restrt = .false.
          ivbact = .false.
      endif

      timep = time - dt
      call IVBCLS(time   ,timep  ,ngrid  ,nbran  ,branch  ,
     +            x      ,gridnm ,qp(1,3),hp(1,3),qp(1,1) )

c     In case of IVBDOS.INI restart will only be written
c     if flag is set

      if (ivbact) wrirst = restrt

      if (maxwl) then
         if (first) then
            kode = 1
         else if (istep.eq.nstep) then
            kode = 3
         else
            kode = 2
         endif
         call IVBOUT(kode  ,ngrid  ,x     ,hp(1,3),hmax   ,
     +               nbran ,branch ) 
      endif

      end
