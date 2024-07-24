      subroutine gsdelt (deposi ,nlayer ,dmed   ,dmexla ,dmed0 ,deffec,
     &                   ddefdd ,radelt ,delta  ,deltb  ,deltc )
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsdelt.F,v $
c Revision 1.2  1995/09/27  10:12:03  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c Module:             GSDELT (Graded Sediment calculate DELTa
c                             coefficients)
c
c     Declaration of parameters
c
      integer    nlayer
      real       dmed   ,dmexla ,dmed0 ,deffec ,ddefdd ,
     &           radelt ,delta  ,deltb ,deltc
      logical    deposi
c
c     Declaration of local variables
c
      real       delte
c
      if (deposi) then
c        Deposition
         delta  = delta + dmed * radelt * ddefdd
      else
c        Erosion
         if (nlayer .eq. 1) then

             delta = delta + dmed0 * radelt * ddefdd
             delte = 1. / (1. + (dmed - dmed0) / deffec * ddefdd)
c            3-1-01 ARS 6025
c            avoid devide by zero  
             if (abs(delte).gt.1000000.) then
                delte = sign(1000000.,delte)
             endif    

         else if (nlayer .eq. 2) then

             delta = delta + dmexla * radelt * ddefdd
             delte = 1. / (1. + (dmed - dmexla) / deffec * ddefdd)

c            3-1-01 ARS 6025
c            avoid devide by zero  
             if (abs(delte).gt.1000000.) then
                delte = sign(1000000.,delte)
             endif    

         endif
         delta = delta * delte
         deltb = deltb * delte
         deltc = deltc * delte
      endif

      end
