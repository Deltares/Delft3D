subroutine mererr ( rectem, recsim, recdis, lendis)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Genral routines module
!
! Programmer:         J.Kuipers
!
! Module:             MERge ERRor message
!
! Module description: The message of the template and the message build
!                     by Sobeksim are merged (i.e. the parameters are
!                     substituted).
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 rectem            I  Record with template of message.
!  2 recsim            I  Record with parameters filled in.
!  3 recdis            O  Record to be displayed.
!  4 lendis            O  Length of recdis
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: mererr.pf,v $
! Revision 1.1  1999/06/01  13:42:29  kuipe_j
! names in messages substituted + message template
!
!
!
!***********************************************************************
!
!     Parameters
!

   character*(*)  rectem, recsim, recdis
   integer        lendis
!
!     Variables
!
   integer        it1, it2, is1, is2, lt, ls ,idis, temlen, simlen,&
   &l  , i
   logical        aap, start

   temlen = len (rectem)
   simlen = len (recsim)
   lt     = 1
   ls     = 1
   it2    = 0
   is2    = 0
   idis   = 0
   aap    = .true.
   start  = .false.

!     Loop until no @ is in record

   do while (aap)
      it1 = it2
      is1 = is2
      it2 = 0
      is2 = 0
      if (lt.le.temlen)&
      &it2 = index (rectem(lt:temlen),'@')
      if (ls.le.simlen)&
      &is2 = index (recsim(ls:simlen),'@')
      aap = it2.ne.0 .and. is2.ne.0
      if (aap) then
         it2 = lt + it2 - 1
         is2 = ls + is2 - 1
         lt  = it2 + 1
         ls  = is2 + 1
         if (start) then

!              Insert parameter

            l   = is2 - is1 - 1
            if (l.gt.0) recdis(idis+1:idis+l) = recsim(is1+1:is2-1)
         else

!              Copy from template

            l   = it2 - it1 - 1
            if (l.gt.0) recdis(idis+1:idis+l) = rectem(it1+1:it2-1)
         endif
         start = .not. start
      else
         it2  = temlen + 1
         l    = it2 - it1 -1
         if (l.gt.0) recdis(idis+1:idis+l) = rectem(it1+1:it2-1)
      endif
      idis  = idis + l
   enddo

!     remove trailing blanks

   l = idis
   do i=idis,1,-1
      if (recdis(i:i).ne.' ') exit
      l = l - 1
   enddo

   lendis = l

   return
end
