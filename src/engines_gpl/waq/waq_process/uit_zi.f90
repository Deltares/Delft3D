!!  Copyright (C)  Stichting Deltares, 2012-2026.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module m_uitzicht_spectrum
    use m_waq_precision

    !***********************************************************************
    !     spectrale gegevens van 400 tot 700 nm met stappen van 5 nm
    !***********************************************************************
    !
    private
    public :: num_spectrum, uit_zi

    integer, parameter :: num_spectrum = 61
    !
    !     specifieke absorptie algen
    !
    real(kind = dp), dimension(num_spectrum) :: chlspe = &
           [0.685, 0.781, 0.828, 0.883, 0.913, &
            0.939, 0.973, 1.001, 1.000, 0.971, &
            0.944, 0.928, 0.917, 0.902, 0.870, &
            0.839, 0.798, 0.773, 0.750, 0.717, &
            0.688, 0.645, 0.618, 0.582, 0.528, &
            0.504, 0.474, 0.444, 0.416, 0.384, &
            0.357, 0.321, 0.294, 0.273, 0.276, &
            0.278, 0.281, 0.279, 0.270, 0.252, &
            0.256, 0.262, 0.261, 0.268, 0.281, &
            0.299, 0.316, 0.328, 0.329, 0.337, &
            0.361, 0.397, 0.457, 0.529, 0.556, &
            0.534, 0.485, 0.411, 0.334, 0.270, &
            0.215]
    !
    !     spectrale verdeling invallend licht
    !
    real(kind = dp), dimension(num_spectrum) :: planck = &
           [0.18380, 0.18836, 0.19280, 0.19710, 0.20134, &
            0.20542, 0.20937, 0.21320, 0.21689, 0.22046, &
            0.22389, 0.22720, 0.23037, 0.23342, 0.23630, &
            0.23912, 0.24177, 0.24430, 0.24671, 0.24898, &
            0.25115, 0.25320, 0.25511, 0.25691, 0.25860, &
            0.26018, 0.26166, 0.26300, 0.26427, 0.26540, &
            0.26650, 0.26745, 0.26830, 0.26910, 0.26978, &
            0.27037, 0.27089, 0.27132, 0.27167, 0.27194, &
            0.27214, 0.27226, 0.27232, 0.27231, 0.27223, &
            0.27209, 0.27189, 0.27163, 0.27130, 0.27094, &
            0.27050, 0.27004, 0.26950, 0.26894, 0.26833, &
            0.26767, 0.26697, 0.26620, 0.26545, 0.26464, &
            0.26379]
    !
    !     verstrooiing water
    !
    real(kind = dp), dimension(num_spectrum) :: bwater = &
           [0.005290, 0.005025, 0.004776, 0.004543, 0.004323, &
            0.004117, 0.003922, 0.003739, 0.003567, 0.003404, &
            0.003250, 0.003105, 0.002968, 0.002838, 0.002715, &
            0.002599, 0.002488, 0.002384, 0.002285, 0.002191, &
            0.002102, 0.002017, 0.001936, 0.001860, 0.001787, &
            0.001717, 0.001651, 0.001588, 0.001528, 0.001471, &
            0.001416, 0.001364, 0.001314, 0.001267, 0.001221, &
            0.001178, 0.001136, 0.001097, 0.001059, 0.001022, &
            0.000987, 0.000954, 0.000922, 0.000891, 0.000862, &
            0.000833, 0.000806, 0.000780, 0.000755, 0.000731, &
            0.000708, 0.000686, 0.000664, 0.000644, 0.000624, &
            0.000605, 0.000587, 0.000569, 0.000552, 0.000536, &
            0.00052]
    !
    !     absorptie water
    !
    real(kind = dp), dimension(num_spectrum) :: awater = &
           [0.00576, 0.00617, 0.00669, 0.00727, 0.00790, &
            0.00854, 0.00918, 0.00980, 0.01039, 0.01093, &
            0.01144, 0.01193, 0.01241, 0.01293, 0.01353, &
            0.01426, 0.01520, 0.01645, 0.01810, 0.02032, &
            0.02380, 0.02818, 0.03294, 0.03768, 0.04212, &
            0.04604, 0.04937, 0.05211, 0.05438, 0.05639, &
            0.05849, 0.06108, 0.06470, 0.06999, 0.07768, &
            0.08886, 0.10519, 0.12723, 0.15488, 0.18723, &
            0.22238, 0.25724, 0.28740, 0.30690, 0.31434, &
            0.31761, 0.31940, 0.32232, 0.32862, 0.33985, &
            0.35650, 0.37763, 0.40052, 0.42033, 0.43334, &
            0.44359, 0.45513, 0.47316, 0.50342, 0.55154, &
            0.62200]

contains

subroutine uit_zi (diep1, diep2, angle, c_gl1, c_gl2, &
        c_det, helhum, tau, corchl, chloro, &
        detrit, gloeir, ah_380, secchi, d_1, &
        extpar, extp_d, dosecc, spectrum, swspec,
     &                    swkd)
    !>\file
    !>       transparency due to chlorophyll, detritus, inorganics and humic accids

    !***********************************************************************
    !     subroutine uit_zi, gebaseerd op het model uitzicht van
    !     h. buiteveld, riza, postbus 17, 8200 aa lelystad (tel 03200-70737)
    !
    !     berekend doorzicht en extinktie op basis van chlorofyl, detritus
    !     gloeirest, absorptie humuszuren bij 380 nm en spectra om de 5 nm
    !     van de absorptie en verstrooiing van water, de verdeling de
    !     invallend licht en de specifieke specifieke absorptie algen
    !
    !     changes in the module:
    !     date   author          description
    !     ------ --------------- -------------------------------------------
    !     971119 jan van beek    extended ascii characters verwijderd
    !     970127 rik sonneveldt  in subroutine bep_d beveiliging tegen chloro
    !                            < 0 ingebouwd (op verzoek van maarten ouboter).
    !     911125 andre hendriks  code beter leesbaar gemaakt, en variabele-
    !                            namen langer dan 6 letters vervangen door korte
    !                            namen.
    !     910926 wolf mooij      implementation in delwaq-bloom
    !                            belangrijkste verandering: inplaats van
    !                            totaal zwevend stof wordt nu direct
    !                            de detritus concentratie ingelezen
    !***********************************************************************
    !***********************************************************************
    !     arguments:
    !     name   type size   i/o description
    !     ------ ---- ------ --- -------------------------------------------
    !     angle   r*8        in  function, default constant = 30 x
    !     ah_380  r*8        in  parameter: extinctie humuszuren (1/m)
    !     chloro  r*8        in  parameter: chlorophyl (mg/m3)
    !     corchl  r*8        in  constant, default = 2.5
    !     c_det   r*8        in  constant, default = 0.026
    !     c_gl1   r*8        in  constant, default = 0.73
    !     c_gl2   r*8        in  constant, default = 1.0
    !     detrit  r*8        in  parameter: gesuspendeerd detritus (gdw/m3)
    !     diep1   r*8        in  constant, default = 1.0 (m)
    !     diep2   r*8        in  constant, default = 1.2 (m)
    !     d_1     r*8        out dummy parameter d 10% transmis 560 nm (m)
    !     extpar  r*8        out parameter: extinctie op 1m (1/m)
    !     extp_d  r*8        out dummy parameter extinctie op d_1 (1/m)
    !     gloeir  r*8        in  parameter: anorganisch zwevend stof (gdw/m3)
    !     helhum  r*8        in  constant, default = 0.014
    !     secchi  r*8        out parameter: doorzicht (m)
    !     tau     r*8        in  constant, default = 7.8
    !***********************************************************************
    !***********************************************************************
    !     common variables:
    !     name   type size   description
    !     ------ ---- ------ -----------------------------------------------
    !     awater  r*8 (61)   absorptie water
    !     bwater  r*8 (61)   verstrooing water
    !     chlspe  r*8 (61)   specifieke absorptie algen
    !     planck  r*8 (61)   verdeling invallend licht
    !***********************************************************************
    !***********************************************************************
    !     local variables:
    !     name   type size   description
    !     ------ ---- ------ -----------------------------------------------
    !     a       r*8
    !     a_chl   r*8
    !     a_det   r*8
    !     a_hum   r*8
    !     b       r*8
    !     b_chl   r*8
    !     b_gl    r*8
    !     c_chl   r*8
    !     c_gl    r*8
    !     c_mu    r*8
    !     d_2     r*8
    !     ext_ki  r*8
    !     i_550   i*8
    !     lambda  i*8
    !     som_c   r*8
    !     som_d1  r*8
    !     som_d2  r*8
    !     som_h   r*8
    !     s_d1    r*8
    !     s_d2    r*8
    !     teller  i*8
    !     zw_stf  r*8
    !***********************************************************************
    !
    real(kind = dp) :: a, a_chl, a_det, a_hum, ah_380, &
            angle, b, b_chl, b_gl, &
            c_chl, c_det, c_gl, c_gl1, c_gl2, &
            c_mu, chloro, corchl, d_1, d_2, &
            detrit, diep1, diep2, ext_ki, extp_d, &
            extpar, gloeir, helhum, s_d1, s_d2, &
            secchi, som_c, som_d1, som_d2, som_h, &
            tau, zw_stf
    real(kind = dp) :: spectrum(:)
    integer(kind = int_wp) :: i_550, lambda, teller
    integer(kind = int_wp) :: dosecc, swkd

    c_mu = cos (angle * 0.0174533)

    if (dosecc) &
            call bep_d  (c_gl1, c_gl2, c_det, helhum, corchl, &
                    c_mu, chloro, detrit, gloeir, ah_380, &
                    d_1, d_2)
    i_550 = ((550 - 400) / 5) + 1
    !
    !        chlorofyl bundelverzwakking
    !
    c_chl = (0.058 + 0.018 * chloro) * chlspe (i_550)
    c_chl = (c_chl + 0.12 * (chloro**0.63)) * corchl
    som_d1 = 0.0
    som_d2 = 0.0
    s_d1 = 0.0
    s_d2 = 0.0
    som_c = 0.0
    som_h = 0.0

    do teller = 1,num_spectrum
        lambda = 400 + (teller - 1) * 5
        !
        !           humuszuren abosorptie
        !
        a_hum = ah_380 * exp (-helhum * (lambda - 380.0))
        !
        !           algen absoroptie en verstrooiing
        !
        if (chloro < 0.000001) then
            a_chl = 0.0
            b_chl = 0.0
        else
            a_chl = (0.058 + 0.018 * chloro) * chlspe (teller) * &
                    corchl
            b_chl = c_chl - a_chl
        endif
        !
        !           gloeirest en detritus
        !
        zw_stf = gloeir + detrit
        c_gl = c_gl1 * ((zw_stf**c_gl2)) * (400.0 / lambda)
        a_det = c_det * detrit * (400.0 / lambda)
        b_gl = c_gl - a_det
        !
        !           totaal absorptie en verstrooiing bij lambda
        !
        a = awater (teller) + a_hum + a_det + a_chl
        b = bwater (teller) + b_gl + b_chl

        !
        ! extinktie bij lambda
        !
        ! choose the calculation of the kd
        !   0   = do not use uizicht
        !   <>0 = use uitzicht
        !   1: buiteveld z1%
        !   2: buiteveld z10%
        !   3: lee with zenith angle 30 eq 4  using 4x m
        !   4: lee with zenith angle 30 eq 5  using 4x n
        !   5: lee simplified eq 6 with 2 coeffs
        !   6: nechad and ruddick 2010
        !   7: nechad and ruddick 2010 reduced
        !
        select ( swkd ) then
            case( 1 ) ! buiteveld z1%
               ext_ki = 1.0 / c_mu * sqrt ( a**2 + (0.425 * c_mu - 0.19) * a * b)

            case( 2 ) ! buiteveld z10%
               ext_ki = 1.0 / c_mu * sqrt ( a**2 + (0.473 * c_mu - 0.218) * a * b)

            case( 3 ) ! lee with zenith angle 30 eq 4  using 4x m
               m0 = 1.108
               m1 = 4.245
               m2 = 0.526
               m3 = 10.942
               ! check if angle is correctly used
               ext_ki = m0 * a + m1 * (1-m2*exp(-m3*a))*b

            case( 4 ) ! lee with zenith angle 30 eq 5  using 4x n
               m0 = 0.005
               m1 = 4.18
               m2 = 0.52
               m3 = 10.8
               ! check if angle is correctly used
               ext_ki = (1 + m0 * angle) * a + m1 * (1-m2*exp(-m3*a))*b

            case( 5 ) ! lee simplified eq 6 with 2 coeffs
               m0 = 0.005
               m4 = 3.47
               ! check if angle is correctly used
               ext_ki = (1 + m0 * angle) * a + m4 * b

            case( 6 ) ! nechad and ruddick 2010
               cc= 0.5 ! no actual cloudcover yet
               anglepi = angle/360.0 * 2 * pi
               m0 = 1.09 + 0.49 * cosh (anglepi)*cosh(0.7*cc)-0.56*(anglepi*cc)
               m1=m0*m0*4.266-4.56*cosh(anglepi)*cosh(0.73*cc)+5.51*cosh(anglepi*cc)
               m2 = m1
               ext_ki = m0 * a + m1 * b - m2 * b*b/a

            case( 7 ) ! nechad and ruddick 2010 reduced
               m0 = 1.1
               m4 = 4.5
               m5 = -3.1
               ext_ki = m0 * a + m4 * b + m5 * b*b/a

            case default
                ! no uitzicht
        end select

        sextdiep1 = spectrum(teller) * exp ( -ext_ki * diep1)

        som_d1 = som_d1 + max(sextdiep1,1.e-30)
        sextdiep2 = spectrum ( teller) * exp ( -ext_ki * diep2)
        som_d2 = som_d2 + max(sextdiep2,1.e-30)


        if ( dosecc >= 1 ) then
            !switch bepaalt welke diepte wordt gebruikt............................
            if ( dosecc >= 2 ) then ! secchi op basis van vaste diepte
               s_d1   = s_d1   + spectrum ( teller) * exp ( -ext_ki * d_1)
               s_d2   = s_d2   + spectrum ( teller) * exp ( -ext_ki * d_2)
               som_h  = som_h  + spectrum ( teller) * exp ( -(a+b)  * d_1)
               som_c  = som_c  + spectrum ( teller) * exp ( -(a+b)  * d_2)
            else
               s_d1   = s_d1   + spectrum ( teller) * exp ( -ext_ki * diep1)
               s_d2   = s_d2   + spectrum ( teller) * exp ( -ext_ki * diep2)
               som_h  = som_h  + spectrum ( teller) * exp ( -(a+b)  * diep1)
               som_c  = som_c  + spectrum ( teller) * exp ( -(a+b)  * diep2)
            endif
        endif

        !
        ! restant van spectrum aan bodem van segment
        ! skip if uitzicht in mode to calculate contribution of an individual oas (sw=0)
        ! in that case provide spectrum based on actual composition (all oas) at top of segment
        !
        if (swspec .eq. 1) then
            spectrum (teller) =sextdiep2
        endif

     enddo

     !
     ! at too small values of (sum of) spectral extinction (sextdiepx) abort the calculation
     ! check for minimum size som_d1 and som_d2 at the two depth'
     ! not sufficient to check their ratio!
     !
     if ( som_d2 > 1e-20 .and. som_d1 > 1e-20 ) then
        extpar = 1.0 / ( diep1 - diep2) * log ( som_d2 / som_d1)
     else
        extpar = -1
     endif


     if ( dosecc >= 1 .and. extpar > -0.5 ) then
         !
         ! bepaal  de secchidiepte midden in segment (diep1,diep2) met lokale extpar
         ! dit is een horizontaal zicht (secchih)
         !
         if ( dosecc >= 1 ) then ! secchi op basis van vaste diepte
             extp_d = 1.0 / ( d_1   - d_2  ) * log ( s_d2   / s_d1  )
             som_c  = 1.0 / ( d_1   - d_2  ) * log ( som_c  / som_h )
             secchi = tau / ( extp_d + som_c)
         else                      ! secchi op basis van lokale diepte in segment
             extp_d = 1.0 / ( diep1   - diep2  ) * log ( s_d2   / s_d1  )
             som_c  = 1.0 / ( diep1   - diep2  ) * log ( som_c  / som_h )
             secchi = tau / ( extp_d + som_c)
         endif
     else
         secchi = -1.0
     endif

end subroutine uit_zi

subroutine bep_d (c_gl1, c_gl2, c_det, helhum, corchl, &
        c_mu, chloro, detrit, gloeir, ah_380, &
        d_1, d_2)
    !***********************************************************************
    !     bepaalt diepte waar 10 % van het licht over is bij 550  nm
    !***********************************************************************
    !***********************************************************************
    !     arguments:
    !     name   type size   i/o description
    !     ------ ---- ------ --- -------------------------------------------
    !     ah_380  r*8        in  parameter: extinctie humuszuren (1/m)
    !     chloro  r*8        in  parameter: chlorophyl (fg/l)
    !     corchl  r*8        in  constant,  default  =  2.5
    !     c_det   r*8        in  constant,  default  =  0.026
    !     c_gl1   r*8        in  constant,  default  =  0.73
    !     c_gl2   r*8        in  constant,  default  =  1.0
    !     c_mu    r*8        in  constant,  cosinus van angle (default  =  30)
    !     detrit  r*8        in  parameter: gesuspendeerd detritus (mg/l)
    !     d_1     r*8        out dummy parameter d 10% transmis 560 nm (m)
    !     d_2     r*8        out dummy parameter d_1 + 0.1 (m)
    !     gloeir  r*8        in  parameter: anorganisch zwevend stof (mg/l)
    !     helhum  r*8        in  constant,  default  =  0.014
    !***********************************************************************
    !***********************************************************************
    !     common variables:
    !     name   type size   description
    !     ------ ---- ------ -----------------------------------------------
    !     awater  r*8 (61)   absorptie water
    !     bwater  r*8 (61)   verstrooing water
    !     chlspe  r*8 (61)   specifieke absorptie algen
    !     planck  r*8 (61)   verdeling invallend licht
    !***********************************************************************
    !***********************************************************************
    !     local variables:
    !     name   type size   description
    !     ------ ---- ------ -----------------------------------------------
    !     a      r*8
    !     a_chl  r*8
    !     a_det  r*8
    !     a_hum  r*8
    !     b      r*8
    !     b_chl  r*8
    !     b_gl   r*8
    !     c_chl  r*8
    !     c_gl   r*8
    !     ext_ki r*8
    !     i_550  i*8
    !     lambda i*8
    !     teller i*8
    !     zw_stf r*8
    !***********************************************************************


    real(kind = dp) :: a, a_chl, a_det, a_hum, ah_380, &
            b, b_chl, b_gl, c_chl, c_det, &
            c_gl, c_gl1, c_gl2, c_mu, chloro, &
            corchl, d_1, d_2, detrit, ext_ki, &
            gloeir, helhum, zw_stf
    integer(kind = int_wp) :: i_550, lambda, teller

    i_550 = ((550 - 400) / 5) + 1

    !
    !     beveiliging tegen negatieve waarde chloro (rs27jan97 voor maarten o.)
    !
    chloro = max(0.0_dp, chloro)
    !
    !     chlorofyl bundel verzwakking
    !
    c_chl = (0.058 + 0.018 * chloro) * chlspe (i_550)
    c_chl = (c_chl + 0.12 * (chloro**0.63)) * corchl
    teller = ((560 - 400) / 5) + 1
    lambda = 400 + (teller - 1) * 5

    !
    !     absorptie humuszuren
    !
    a_hum = ah_380 * exp (-helhum * (lambda - 380))

    !
    !     chlorofyl
    !
    if (chloro < 0.000001) then
        a_chl = 0.0
        b_chl = 0.0
    else
        a_chl = (0.058 + 0.018 * chloro) * chlspe (teller) * corchl
        b_chl = c_chl - a_chl
    endif

    !
    !     gloeirest en detritus
    !
    zw_stf = gloeir + detrit
    c_gl = c_gl1 * (zw_stf**c_gl2) * (400.0 / lambda)
    a_det = c_det * detrit * (400.0 / lambda)
    b_gl = c_gl - a_det
    a = awater (teller) + a_hum + a_det + a_chl
    b = bwater (teller) + b_gl + b_chl
    ext_ki = 1 / c_mu * &
            sqrt ((a * a + (0.425 * c_mu - 0.19) * a * b))

    !
    !     diepte 10 % transmissie 560 nm
    !
    d_1 = 2.3 / ext_ki
    d_2 = d_1 + 0.1

end subroutine bep_d

end module m_uitzicht_spectrum
