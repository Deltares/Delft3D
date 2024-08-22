      subroutine FLQHT(h1 ,h2 ,q      ,qdh1   ,qdh2 ,s    ,xnod  ,ynod ,
     &                 n  ,m  ,deltah ,inttyp ,zs   ,zero ,rlimit)
c      
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         H.Petit
c
c Module:             FLQHT (FLow QH relation for Tabulated structure)
c
c Module description: Subroutine FLQHT approximates from  Q-h table to
c                     determine values for Q, (dQ/dh1) and (dQ/dh2).
c
c                     Bilinear interpolation is used. Near the lines where
c                     derivatives become discontinuous smoothing is used to
c                     reduce this effect.
c-----------------------------------------------------------------------
c Parameters:
c NAME             IO DESCRIPTION
c deltah           I  switch for type of arguments ynod:
c                     true:  ynod = h1-h2
c                     false: ynod = h2
c inttyp           I  1 = linear interpolation
c                     2 = spline 
c h1               I  water level at the left  of the structure
c h2               I  water level at the right of the structure
c lim(4)           O  water levels in case of excedence of matrix
c m                I  number of h2 (or h1-h2) values-1 in the table
c                     given in the array ynod  
c n                I  number of h1 values-1 in the table given in
c                     the array xnod
c q                O  resulting Q value for given h1 and h2
c qdh1             O  (dQ/dh1) determined for approximation function
c qdh2             O  (dQ/dh2) determined for approximation function
c s                I  Table containing discharges Q of discrete 
c                     Q-h relation.
c                     s(i,j) = Q(h1(i),h2(j))      or
c                              Q(h1(i),(h1-h2)(j))     for i=0(1)n
c                                                  and for j=0(1)m
c xnod             I  xnod(j)= h1(i) for i=0(1)n 
c ynod             I  ynod(j)= h2(j) or (h1-h2)(j) for j=0(1)m
c zero             I  if .true. discharge will be set zero if sill
c                     is dry.
c-----------------------------------------------------------------------
c
c     Declaration of parameters
c
      integer n   ,m   ,inttyp
      real    h1  ,h2  ,q    ,zs ,qdh1 ,qdh2 ,bigd
      real    rlimit   ,s(0:n,0:m)
      real    xnod(0:n),ynod(0:m)
      logical deltah   ,zero
c      
c     Declaration of local variables
c
      integer nmax   ,mmax   ,limit
      real    x      ,y      ,dxf   ,dyf
      logical xunder ,yunder ,xover ,yover 
c      
      bigd =10.0
      nmax = n
      mmax = m
c
c     if deltah=.true. then ynod represents values for h1-h2
c     else ynod represents values for h2
c
      x=h1-zs
      if(deltah)then
        y=h1-h2
      else  
        y=h2-zs
      endif  
c 
c     If a crest level is detected h1 and h2 should not be
c     below crest
c
         if (.not.deltah) x = max(x,0.)
         if (.not.deltah) y = max(y,0.)
c
      if (inttyp.eq.1) then
         call intlin(nmax ,mmax ,n   ,m   ,s   ,xnod   ,ynod   ,bigd  ,
     +               x    ,y    ,q   ,dxf ,dyf ,xunder ,yunder ,xover ,
     +               yover)
      else
         write (*,*) 'FLQHT: Unknown interpolation method'
         stop
      endif
c
c     Calculate linearization coefficients
c
      if (h1.lt.zs .and. h2.lt.zs) then
c       below crest 
        q    = 0.
        qdh1 = 0.
        qdh2 = 0.
      else
c       above crest or cest not detected      
        if(deltah)then
          qdh1 = dxf+dyf
          qdh2 = -dyf
        else
          qdh1 = dxf
          qdh2 = dyf
        endif
      endif
c     
c     Set matrix overflow indicator
c
      limit = 0
      if (.not.zero) then
        if (xunder) limit = limit + 1 
        if (yunder) limit = limit + 10
      endif
      if (xover) limit = limit + 100 
      if (yover) limit = limit + 1000
      rlimit = limit + .1
c
      return
      end
