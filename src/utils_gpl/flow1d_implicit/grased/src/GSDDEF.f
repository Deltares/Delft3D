      subroutine gsddef (heiopt ,lathic ,g      ,relden ,kinvis ,dmed  ,
     &                   chezy  ,u      ,depth  ,frou2  ,duncof,
     &                   trforb ,duncon ,sedexp ,deff0  ,ddefdh ,ddefdu,
     &                   ddefdd ,redfac )
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsddef.F,v $
c Revision 1.3  1996/06/07  11:56:00  kuipe_j
c multi  +  fixed layer
c
c Revision 1.2  1995/09/27  10:12:01  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c Module:             GSDDEF (Graded Sediment calculate Derivatives of
c                            DEFfective)
c
c     Declaration of parameters
c
      integer    heiopt ,lathic
      real       g      ,relden ,kinvis ,dmed   ,chezy   ,
     &           u      ,depth  ,frou2  ,sedexp ,redfac  ,
     &           deff0  ,ddefdh ,ddefdu ,ddefdd
      real       trforb(*)      ,duncof(*)      ,duncon(*)
c
c     Declaration of local variables
c
      real       velo  ,velo1 ,dvelo ,depth1, ddepth, dmed1, ddmed ,
     &           deffec,dunehe,frou21
      logical    initra
c
c     Constants
c
      real       diff
      parameter  (diff=.001)

      initra = .false.
c
c     Numerical differentiation of layer thickness to u, h and Dm
c
c     Calculate |h+dh|
c
      ddepth = depth * diff
      depth1 = depth + ddepth
      velo   = abs(u)
      frou21 = velo**2 / (g*depth1)
c
c     Calculate layer thickness based on h+dh
c
      call gsdunh (initra ,heiopt ,g      ,relden ,kinvis ,dmed  ,
     &             chezy  ,velo   ,depth1 ,frou21 ,duncof ,trforb,
     &             duncon ,sedexp ,dunehe )

      deffec = deff0
      call gslati (lathic ,dunehe ,deffec ,redfac ,dmed)
      ddefdh = (deffec - deff0) / ddepth
c
c     Calculate |u+du|
c
      dvelo  = max(velo * diff,diff)
      velo1  = velo + dvelo
      frou21 = velo1**2 / (g*depth)
c
c     Calculate layer thickness based on u+du
c
      call gsdunh (initra ,heiopt ,g      ,relden ,kinvis ,dmed  ,
     &             chezy  ,velo1  ,depth  ,frou21 ,duncof ,trforb,
     &             duncon ,sedexp ,dunehe )

      call gslati (lathic ,dunehe ,deffec ,redfac ,dmed)

      ddefdu = sign((deffec - deff0) / dvelo , u)
c
c     Calculate |Dm+dDm|
c
      ddmed = dmed * diff
      dmed1 = dmed + ddmed
c
c     Calculate layer thickness based on Dm+dDm
c
      call gsdunh (initra ,heiopt ,g      ,relden ,kinvis ,dmed1 ,
     &             chezy  ,velo   ,depth  ,frou2  ,duncof ,trforb,
     &             duncon ,sedexp ,dunehe )

      call gslati (lathic ,dunehe ,deffec ,redfac ,dmed1)

      ddefdd = (deffec - deff0) / ddmed
c
      return
c
      end
