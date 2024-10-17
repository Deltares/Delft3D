subroutine normp(p,np,nfa,eps,gperr)
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: normp.F,v $
! Revision 1.3  1996/06/07  11:56:46  kuipe_j
! multi  +  fixed layer
!
! Revision 1.2  1995/09/27  10:13:05  kuipe_j
! Maintenance
!
!
!***********************************************************************
   integer   np,nfa,gperr
   real      eps
   real      p(np,*)

   integer   nfmax
   parameter(nfmax=100)
   integer   i,j,it,nf
   integer   ip(nfmax)
   real      h

   if(nfa.gt.nfmax) stop 'nf > nfmax'
   if(nfa*eps.gt.1.) stop 'nf*eps > 1'
! zorg dat de som van de p's 1 blijft
   gperr = 0
   do 10 i=1,np
      it=0
      nf= nfa
!f    if(p(i,nfa).lt.1.0e-15) nf=nfa-1
      if(p(i,nfa).lt.1.0e-15) then
         nf=nfa-1
         p(i,nfa)= 0.0
      endif
!f
      do 20 j=1,nf
         ip(j)=0
         if(p(i,j).ge.eps) goto 20
         ip(j)=1
         it=it+1
         p(i,j)=eps
20    continue
!     check if it = nf
      if(it.eq.nf) then
         gperr = i
         goto 100
      endif
      h=0.0
      do 50 j=1,nf
         if(ip(j).eq.1) goto 50
         h=h+p(i,j)-eps
50    continue
      h=(1.-nf*eps)/h
      do 60 j=1,nf
         if(ip(j).eq.1) goto 60
         p(i,j)=eps+(p(i,j)-eps)*h
60    continue
10 continue
100 continue
   return
end
