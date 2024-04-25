!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

double precision function tabellenboek(d1        ,eweir     ,qunit     ,qvolk     )
!-------------------------------------------------------------------------------
!!--description-----------------------------------------------------------------
!
!    Function: Determines additional energy loss due to weir.
!              Energy loss dependent on velocity perpendicular
!              to weir (Schonfeld, 1955).
!              Subroutine based on subroutine TABEL in WAQUA.
!              Energyloss based on Carnot relation for
!              decellerated flow, or on "Tabellenboek".
!              This subroutine is also called for supercritical
!              flow (qqv = 1.0), see the formulations in
!              "Beoordeling nieuwe overlaatroutines WAQUA"
!              WL-report Z3063.
! Method used: Reference : Weergave van extra energieverlies
!              in RIVCUR. (J.H.A. Wybenga, report
!              Deltares Q779 voor RWS/RIZA, 1989).
!              Energieverliezen door overlaten: een gewijzigde
!              berekeningsprocedure voor WAQUA-rivieren versie.
!              (H. Vermaas, verslag onderzoek Deltares
!               Q92, 1987)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    real(fp)          , intent(in) :: d1     !!  Distance between crest and downstream depth
    real(fp)          , intent(in) :: eweir  !!  Energy level at weir
    real(fp)          , intent(in) :: qunit  !!  Discharge at weir crest
    real(fp)          , intent(in) :: qvolk  !!  Maximum discharge (super critical flow)

!
!
! Local variables
!
    real(fp)                       :: f1
    real(fp)                       :: f1low
    real(fp)                       :: f1up
    real(fp)                       :: f2
    real(fp)                       :: f2low
    real(fp)                       :: f2up
    real(fp)                       :: qqv
    real(fp)                       :: qqvlow
    real(fp)                       :: qqvup
    real(fp)                       :: theta
!
!
!! executable statements -------------------------------------------------------
!
    !
    qqv = min(qunit/qvolk, 1.0_fp)
    !
    !-----Calculate energy loss according to tables (based on experinments
    !     in the delta flums)
    !
    if (qqv<0.3) then
       qqvlow = 0.
       qqvup = 0.3
       if (eweir<0.5) then
          f1low = 0.0
          f1up = 0.0092*eweir
          f2low = 0.0
          f2up = 0.328
       else
          f1low = 0.0
          f1up = 0.0057 + 0.002*log10(eweir - 0.21)
          f2low = 0.0
          f2up = 0.314 + 0.0237*eweir**0.808
       endif
    !
    elseif (qqv<0.6) then
       qqvlow = 0.3
       qqvup = 0.6
       if (eweir<0.5) then
          f1low = 0.0092*eweir
          f1up = 0.0211*eweir
          f2low = 0.328
          f2up = 0.316
       else
          f1low = 0.0057 + 0.002*log10(eweir - 0.21)
          f1up = -0.0017 + 0.033*log10(eweir + 1.85)
          f2low = 0.314 + 0.0237*eweir**0.808
          f2up = 0.283 + 0.0531*eweir**0.702
       endif
    !
    elseif (qqv<0.8) then
       qqvlow = 0.6
       qqvup = 0.8
       if (eweir<0.5) then
          f1low = 0.0211*eweir
          f1up = 0.0463*eweir
          f2low = 0.316
          f2up = 0.295
       else
          f1low = -0.0017 + 0.033*log10(eweir + 1.85)
          f1up = -0.0022 + 0.064*log10(eweir + 1.99)
          f2low = 0.283 + 0.0531*eweir**0.702
          f2up = 0.280 + 0.0272*eweir**0.912
       endif
    !
    elseif (qqv<0.9) then
       qqvlow = 0.8
       qqvup = 0.9
       if (eweir<0.5) then
          f1low = 0.0463*eweir
          f1up = 0.0657*eweir
          f2low = 0.295
          f2up = 0.238
       else
          f1low = -0.0022 + 0.064*log10(eweir + 1.99)
          f1up = 0.039 + 0.076*log10(eweir + 0.33)
          f2low = 0.280 + 0.0272*eweir**0.912
          f2up = 0.224 + 0.0256*eweir**0.869
       endif
    !
    elseif (qqv<0.95) then
       qqvlow = 0.9
       qqvup = 0.95
       if (eweir<0.5) then
          f1low = 0.0657*eweir
          f1up = 0.0772*eweir
          f2low = 0.238
          f2up = 0.206
       else
          f1low = 0.039 + 0.076*log10(eweir + 0.33)
          f1up = 0.065 + 0.116*log10(eweir + 0.092)
          f2low = 0.224 + 0.0256*eweir**0.869
          f2up = 0.124 + 0.11*eweir**0.422
       endif
    !
    elseif (qqv<0.99) then
       qqvlow = 0.95
       qqvup = 0.99
       if (eweir<0.5) then
          f1low = 0.0772*eweir
          f2low = 0.206
       else
          f1low = 0.065 + 0.116*log10(eweir + 0.092)
          f2low = 0.124 + 0.11*eweir**0.422
       endif
       if (eweir<1.0) then
          f1up = 0.115*eweir**0.9
          f2up = 0.242
       else
          f1up = 0.133 + 0.213*log10(eweir - 0.173)
          f2up = 0.133 + 0.109*eweir**0.619
       endif
    !
    elseif (qqv>=0.99) then
       qqvlow = 0.99
       qqvup = 1.0
       if (eweir<1.0) then
          f1low = 0.115*eweir**0.9
          f2low = 0.242
       else
          f1low = 0.133 + 0.213*log10(eweir - 0.173)
          f2low = 0.133 + 0.109*eweir**0.619
       endif
       if (eweir<2.0) then
          f1up = 0.156*eweir**0.75
          f2up = 0.343
       else
          f1up = 0.244 + 0.172*log10(eweir - 0.718)
          f2up = 0.075 + 0.176*eweir**0.609
       endif
    !
    else
    endif
    !
    !-----Calculate terms for energy loss
    !
    theta = (qqv - qqvlow)/(qqvup - qqvlow)
    f1 = (1 - theta)*f1low + theta*f1up
    f2 = (1 - theta)*f2low + theta*f2up
    tabellenboek = f1*d1**f2
end function tabellenboek
