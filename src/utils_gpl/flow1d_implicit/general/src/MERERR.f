      subroutine mererr ( rectem, recsim, recdis, lendis)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Genral routines module
c
c Programmer:         J.Kuipers
c
c Module:             MERge ERRor message
c
c Module description: The message of the template and the message build
c                     by Sobeksim are merged (i.e. the parameters are
c                     substituted).
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 rectem            I  Record with template of message.
c  2 recsim            I  Record with parameters filled in.
c  3 recdis            O  Record to be displayed.
c  4 lendis            O  Length of recdis
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: mererr.pf,v $
c Revision 1.1  1999/06/01  13:42:29  kuipe_j
c names in messages substituted + message template
c
c
c
c***********************************************************************
c
c     Parameters
c

      character*(*)  rectem, recsim, recdis
      integer        lendis
c
c     Variables
c
      integer        it1, it2, is1, is2, lt, ls ,idis, temlen, simlen,
     +               l  , i
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

c     Loop until no @ is in record

      do while (aap)
         it1 = it2
         is1 = is2
	 it2 = 0
	 is2 = 0
         if (lt.le.temlen)
     +   it2 = index (rectem(lt:temlen),'@')
         if (ls.le.simlen)
     +   is2 = index (recsim(ls:simlen),'@')
         aap = it2.ne.0 .and. is2.ne.0
         if (aap) then
            it2 = lt + it2 - 1
            is2 = ls + is2 - 1
            lt  = it2 + 1
            ls  = is2 + 1
            if (start) then

c              Insert parameter

               l   = is2 - is1 - 1
               if (l.gt.0) recdis(idis+1:idis+l) = recsim(is1+1:is2-1)
            else

c              Copy from template

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

c     remove trailing blanks

      l = idis
      do i=idis,1,-1
         if (recdis(i:i).ne.' ') exit
         l = l - 1
      enddo

      lendis = l

      return
      end
