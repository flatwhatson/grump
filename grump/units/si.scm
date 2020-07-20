(define-module (grump units si)
  #:use-module (grump units)
  #:re-export (compatible?
               convert-to)
  #:export-syntax (m kg s A K cd mol Hz N J W Pa rad sr C V F Ω S Wb T H lm lx Bq Gy Sv kat

                   Ym   Zm   Em   Pm   Tm   Gm   Mm   km   hm   dam
                   dm   cm   mm   μm   nm   pm   fm   am   zm   ym
                   Ys   Zs   Es   Ps   Ts   Gs   Ms   ks   hs   das
                   ds   cs   ms   μs   ns   ps   fs   as   zs   ys
                   YA   ZA   EA   PA   TA   GA   MA   kA   hA   daA
                   dA   cA   mA   μA   nA   pA   fA   aA   zA   yA
                   YK   ZK   EK   PK   TK   GK   MK   kK   hK   daK
                   dK   cK   mK   μK   nK   pK   fK   aK   zK   yK
                   Ycd  Zcd  Ecd  Pcd  Tcd  Gcd  Mcd  kcd  hcd  dacd
                   dcd  ccd  mcd  μcd  ncd  pcd  fcd  acd  zcd  ycd
                   Ymol Zmol Emol Pmol Tmol Gmol Mmol kmol hmol damol
                   dmol cmol mmol μmol nmol pmol fmol amol zmol ymol
                   YHz  ZHz  EHz  PHz  THz  GHz  MHz  kHz  hHz  daHz
                   dHz  cHz  mHz  μHz  nHz  pHz  fHz  aHz  zHz  yHz
                   YN   ZN   EN   PN   TN   GN   MN   kN   hN   daN
                   dN   cN   mN   μN   nN   pN   fN   aN   zN   yN
                   YJ   ZJ   EJ   PJ   TJ   GJ   MJ   kJ   hJ   daJ
                   dJ   cJ   mJ   μJ   nJ   pJ   fJ   aJ   zJ   yJ
                   YW   ZW   EW   PW   TW   GW   MW   kW   hW   daW
                   dW   cW   mW   μW   nW   pW   fW   aW   zW   yW
                   YPa  ZPa  EPa  PPa  TPa  GPa  MPa  kPa  hPa  daPa
                   dPa  cPa  mPa  μPa  nPa  pPa  fPa  aPa  zPa  yPa
                   Yrad Zrad Erad Prad Trad Grad Mrad krad hrad darad
                   drad crad mrad μrad nrad prad frad arad zrad yrad
                   Ysr  Zsr  Esr  Psr  Tsr  Gsr  Msr  ksr  hsr  dasr
                   dsr  csr  msr  μsr  nsr  psr  fsr  asr  zsr  ysr
                   YC   ZC   EC   PC   TC   GC   MC   kC   hC   daC
                   dC   cC   mC   μC   nC   pC   fC   aC   zC   yC
                   YV   ZV   EV   PV   TV   GV   MV   kV   hV   daV
                   dV   cV   mV   μV   nV   pV   fV   aV   zV   yV
                   YF   ZF   EF   PF   TF   GF   MF   kF   hF   daF
                   dF   cF   mF   μF   nF   pF   fF   aF   zF   yF
                   YΩ   ZΩ   EΩ   PΩ   TΩ   GΩ   MΩ   kΩ   hΩ   daΩ
                   dΩ   cΩ   mΩ   μΩ   nΩ   pΩ   fΩ   aΩ   zΩ   yΩ
                   YS   ZS   ES   PS   TS   GS   MS   kS   hS   daS
                   dS   cS   mS   μS   nS   pS   fS   aS   zS   yS
                   YWb  ZWb  EWb  PWb  TWb  GWb  MWb  kWb  hWb  daWb
                   dWb  cWb  mWb  μWb  nWb  pWb  fWb  aWb  zWb  yWb
                   YT   ZT   ET   PT   TT   GT   MT   kT   hT   daT
                   dT   cT   mT   μT   nT   pT   fT   aT   zT   yT
                   YH   ZH   EH   PH   TH   GH   MH   kH   hH   daH
                   dH   cH   mH   μH   nH   pH   fH   aH   zH   yH
                   Ylm  Zlm  Elm  Plm  Tlm  Glm  Mlm  klm  hlm  dalm
                   dlm  clm  mlm  μlm  nlm  plm  flm  alm  zlm  ylm
                   Ylx  Zlx  Elx  Plx  Tlx  Glx  Mlx  klx  hlx  dalx
                   dlx  clx  mlx  μlx  nlx  plx  flx  alx  zlx  ylx
                   YBq  ZBq  EBq  PBq  TBq  GBq  MBq  kBq  hBq  daBq
                   dBq  cBq  mBq  μBq  nBq  pBq  fBq  aBq  zBq  yBq
                   YGy  ZGy  EGy  PGy  TGy  GGy  MGy  kGy  hGy  daGy
                   dGy  cGy  mGy  μGy  nGy  pGy  fGy  aGy  zGy  yGy
                   YSv  ZSv  ESv  PSv  TSv  GSv  MSv  kSv  hSv  daSv
                   dSv  cSv  mSv  μSv  nSv  pSv  fSv  aSv  zSv  ySv
                   Ykat Zkat Ekat Pkat Tkat Gkat Mkat kkat hkat dakat
                   dkat ckat mkat μkat nkat pkat fkat akat zkat ykat

                   g L dL cL mL μL min h d deg ha t Å bar mbar atm a b eV meV μeV amu AU)
  #:export (length                  length?
            mass                    mass?
            time                    time?
            electric-current        electric-current?
            temperature             temperature?
            luminous-intensity      luminous-intensity?
            amount-of-substance     amount-of-substance?

            area                    area?
            volume                  volume?
            frequency               frequency?
            velocity                velocity?
            acceleration            acceleration?
            force                   force?
            energy                  energy?
            power                   power?
            pressure                pressure?

            angle                   angle?
            solid-angle             solid-angle?

            electric-charge         electric-charge?
            voltage                 voltage?
            capacitance             capacitance?
            resistance              resistance?
            conductance             conductance?
            magnetic-flux           magnetic-flux?
            magnetic-field-strength magnetic-field-strength?
            inductance              inductance?

            luminous-flux           luminous-flux?
            illuminance             illuminance?
            radioactivity           radioactivity?
            absorbed-dose           absorbed-dose?
            equivalent-dose         equivalent-dose?
            catalytic-activity      catalytic-activity?

            pi))

(define-unit-system SI
  (length              meter     m)
  (mass                kilogram  kg)
  (time                second    s)
  (electric-current    ampere    A)
  (temperature         kelvin    K)
  (luminous-intensity  candela   cd)
  (amount-of-substance mole      mol))

(define-dimensions
  (area                                  (length 2))
  (volume                                (length 3))
  (frequency               hertz     Hz  (time -1))
  (velocity                              (length 1 time -1))
  (acceleration                          (velocity 1 time -1))
  (force                   newton    N   (mass 1 acceleration 1))
  (energy                  joule     J   (mass 1 velocity 2))
  (power                   watt      W   (energy 1 time -1))
  (pressure                pascal    Pa  (force 1 area -1))

  (angle                   radian    rad (length 1 length -1))
  (solid-angle             steradian sr  (area 1 area -1))

  (electric-charge         coulomb   C   (electric-current 1 time 1))
  (voltage                 volt      V   (energy 1 electric-charge -1))
  (capacitance             farad     F   (electric-charge 1 voltage -1))
  (resistance              ohm       Ω   (voltage 1 electric-current -1))
  (conductance             siemens   S   (resistance -1))
  (magnetic-flux           weber     Wb  (energy 1 electric-current -1))
  (magnetic-field-strength tesla     T   (magnetic-flux 1 area -1))
  (inductance              henry     H   (magnetic-flux 1 electric-current -1))

  (luminous-flux           lumen     lm  (luminous-intensity 1 solid-angle 1))
  (illuminance             lux       lx  (luminous-flux 1 area -1))
  (radioactivity           becquerel Bq  (time -1))
  (absorbed-dose           gray      Gy  (energy 1 mass -1))
  (equivalent-dose         sievert   Sv  (energy 1 mass -1))
  (catalytic-activity      katal     kat (amount-of-substance 1 time -1)))

(define-prefixed-units
  (m   meter      s   second   A   ampere  K   kelvin  cd  candela  mol mole
   Hz  hertz      N   newton   J   joule   W   watt    Pa  pascal   rad radian
   sr  steradian  C   coulomb  V   volt    F   farad   Ω   ohm      S   siemens
   Wb  weber      T   tesla    H   henry   lm  lumen   lx  lux      Bq  becquerel
   Gy  gray       Sv  sievert  kat katal)
  (Y  yotta 1000000000000000000000000)
  (Z  zetta 1000000000000000000000)
  (E  exa   1000000000000000000)
  (P  peta  1000000000000000)
  (T  tera  1000000000000)
  (G  giga  1000000000)
  (M  mega  1000000)
  (k  kilo  1000)
  (h  hecto 100)
  (da deca  10)
  (d  deci  1/10)
  (c  centi 1/100)
  (m  milli 1/1000)
  (μ  micro 1/1000000)
  (n  nano  1/1000000000)
  (p  pico  1/1000000000000)
  (f  femto 1/1000000000000000)
  (a  ato   1/1000000000000000000)
  (z  zepto 1/1000000000000000000000)
  (y  yocto 1/1000000000000000000000000))

(define pi 3.14159265358979323846)

(define-units
  (g  gram       (* kg 1/1000))

  (L  liter      (* dm dm dm))
  (dL deciliter  (* L 1/10))
  (cL centiliter (* L 1/100))
  (mL milliliter (* L 1/1000))
  (μL microliter (* L 1/1000))

  (min minute (* 60 s))
  (h   hour   (* 60 min))
  (d   day    (* 24 h))

  (deg degree   (* (/ pi 180) rad))
  (ha  hectare  (* 10000 m m))
  (t   tonne    (* 1000 kg))
  (Å   angstrøm (* 1/10 nm))

  (bar  bar        (* 100000 Pa))
  (mbar millibar   (* 1/1000 bar))
  (atm  atmosphere (* 101325 Pa))

  (a are  (* 100 m m))
  (b barn (* 1/10000 pm pm))

  (eV  electronvolt      (* 1.60217733e-19 C V))
  (meV millielectronvolt (* 1/1000 eV))
  (μeV microelectronvolt (* 1/1000 meV))

  (amu atomic-mass-unit  (* 1.6605402e-27 kg))
  (AU  astronomical-unit (* 1.49597870691e11 m)))
