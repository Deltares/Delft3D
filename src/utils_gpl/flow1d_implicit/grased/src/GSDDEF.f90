subroutine gsddef (heiopt ,lathic ,g      ,relden ,kinvis ,dmed  ,&
&chezy  ,u      ,depth  ,frou2  ,duncof,&
&trforb ,duncon ,sedexp ,deff0  ,ddefdh ,ddefdu,&
&ddefdd ,redfac )
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsddef.F,v $
! Revision 1.3  1996/06/07  11:56:00  kuipe_j
! multi  +  fixed layer
!
! Revision 1.2  1995/09/27  10:12:01  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
! Module:             GSDDEF (Graded Sediment calculate Derivatives of
!                            DEFfective)
!
!     Declaration of parameters
!
   integer    heiopt ,lathic
   real       g      ,relden ,kinvis ,dmed   ,chezy   ,&
   &u      ,depth  ,frou2  ,sedexp ,redfac  ,&
   &deff0  ,ddefdh ,ddefdu ,ddefdd
   real       trforb(*)      ,duncof(*)      ,duncon(*)
!
!     Declaration of local variables
!
   real       velo  ,velo1 ,dvelo ,depth1, ddepth, dmed1, ddmed ,&
   &deffec,dunehe,frou21
   logical    initra
!
!     Constants
!
   real       diff
   parameter  (diff=.001)

   initra = .false.
!
!     Numerical differentiation of layer thickness to u, h and Dm
!
!     Calculate |h+dh|
!
   ddepth = depth * diff
   depth1 = depth + ddepth
   velo   = abs(u)
   frou21 = velo**2 / (g*depth1)
!
!     Calculate layer thickness based on h+dh
!
   call gsdunh (initra ,heiopt ,g      ,relden ,kinvis ,dmed  ,&
   &chezy  ,velo   ,depth1 ,frou21 ,duncof ,trforb,&
   &duncon ,sedexp ,dunehe )

   deffec = deff0
   call gslati (lathic ,dunehe ,deffec ,redfac ,dmed)
   ddefdh = (deffec - deff0) / ddepth
!
!     Calculate |u+du|
!
   dvelo  = max(velo * diff,diff)
   velo1  = velo + dvelo
   frou21 = velo1**2 / (g*depth)
!
!     Calculate layer thickness based on u+du
!
   call gsdunh (initra ,heiopt ,g      ,relden ,kinvis ,dmed  ,&
   &chezy  ,velo1  ,depth  ,frou21 ,duncof ,trforb,&
   &duncon ,sedexp ,dunehe )

   call gslati (lathic ,dunehe ,deffec ,redfac ,dmed)

   ddefdu = sign((deffec - deff0) / dvelo , u)
!
!     Calculate |Dm+dDm|
!
   ddmed = dmed * diff
   dmed1 = dmed + ddmed
!
!     Calculate layer thickness based on Dm+dDm
!
   call gsdunh (initra ,heiopt ,g      ,relden ,kinvis ,dmed1 ,&
   &chezy  ,velo   ,depth  ,frou2  ,duncof ,trforb,&
   &duncon ,sedexp ,dunehe )

   call gslati (lathic ,dunehe ,deffec ,redfac ,dmed1)

   ddefdd = (deffec - deff0) / ddmed
!
   return
!
end
