subroutine FLQHT(h1 ,h2 ,q      ,qdh1   ,qdh2 ,s    ,xnod  ,ynod ,&
&n  ,m  ,deltah ,inttyp ,zs   ,zero ,rlimit)
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         H.Petit
!
! Module:             FLQHT (FLow QH relation for Tabulated structure)
!
! Module description: Subroutine FLQHT approximates from  Q-h table to
!                     determine values for Q, (dQ/dh1) and (dQ/dh2).
!
!                     Bilinear interpolation is used. Near the lines where
!                     derivatives become discontinuous smoothing is used to
!                     reduce this effect.
!-----------------------------------------------------------------------
! Parameters:
! NAME             IO DESCRIPTION
! deltah           I  switch for type of arguments ynod:
!                     true:  ynod = h1-h2
!                     false: ynod = h2
! inttyp           I  1 = linear interpolation
!                     2 = spline
! h1               I  water level at the left  of the structure
! h2               I  water level at the right of the structure
! lim(4)           O  water levels in case of excedence of matrix
! m                I  number of h2 (or h1-h2) values-1 in the table
!                     given in the array ynod
! n                I  number of h1 values-1 in the table given in
!                     the array xnod
! q                O  resulting Q value for given h1 and h2
! qdh1             O  (dQ/dh1) determined for approximation function
! qdh2             O  (dQ/dh2) determined for approximation function
! s                I  Table containing discharges Q of discrete
!                     Q-h relation.
!                     s(i,j) = Q(h1(i),h2(j))      or
!                              Q(h1(i),(h1-h2)(j))     for i=0(1)n
!                                                  and for j=0(1)m
! xnod             I  xnod(j)= h1(i) for i=0(1)n
! ynod             I  ynod(j)= h2(j) or (h1-h2)(j) for j=0(1)m
! zero             I  if .true. discharge will be set zero if sill
!                     is dry.
!-----------------------------------------------------------------------
!
!     Declaration of parameters
!
   integer n   ,m   ,inttyp
   real    h1  ,h2  ,q    ,zs ,qdh1 ,qdh2 ,bigd
   real    rlimit   ,s(0:n,0:m)
   real    xnod(0:n),ynod(0:m)
   logical deltah   ,zero
!
!     Declaration of local variables
!
   integer nmax   ,mmax   ,limit
   real    x      ,y      ,dxf   ,dyf
   logical xunder ,yunder ,xover ,yover
!
   bigd =10.0
   nmax = n
   mmax = m
!
!     if deltah=.true. then ynod represents values for h1-h2
!     else ynod represents values for h2
!
   x=h1-zs
   if(deltah)then
      y=h1-h2
   else
      y=h2-zs
   endif
!
!     If a crest level is detected h1 and h2 should not be
!     below crest
!
   if (.not.deltah) x = max(x,0.)
   if (.not.deltah) y = max(y,0.)
!
   if (inttyp.eq.1) then
      call intlin(nmax ,mmax ,n   ,m   ,s   ,xnod   ,ynod   ,bigd  ,&
      &x    ,y    ,q   ,dxf ,dyf ,xunder ,yunder ,xover ,&
      &yover)
   else
      write (*,*) 'FLQHT: Unknown interpolation method'
      stop
   endif
!
!     Calculate linearization coefficients
!
   if (h1.lt.zs .and. h2.lt.zs) then
!       below crest
      q    = 0.
      qdh1 = 0.
      qdh2 = 0.
   else
!       above crest or cest not detected
      if(deltah)then
         qdh1 = dxf+dyf
         qdh2 = -dyf
      else
         qdh1 = dxf
         qdh2 = dyf
      endif
   endif
!
!     Set matrix overflow indicator
!
   limit = 0
   if (.not.zero) then
      if (xunder) limit = limit + 1
      if (yunder) limit = limit + 10
   endif
   if (xover) limit = limit + 100
   if (yover) limit = limit + 1000
   rlimit = limit + .1
!
   return
end
