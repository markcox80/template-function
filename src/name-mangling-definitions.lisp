(in-package "TEMPLATE-FUNCTION")

(define-type-name-pairs
  (string "S")
  (base-string "Sb" :implementation-specific t))

;; Numbers
(define-type-name-pairs
  (number "N")
  ((eql 0) "kZero")
  ((eql 1) "kOne"))

;; Real Numbers
(define-type-name-pairs
  (real "R")

  (rational "Ra")

  (integer "I")
  (bit "Ib")
  (fixnum "If")

  ((unsigned-byte 2) "Iu2" :implementation-specific t :value #.(1- (expt 2 2)))
  ((unsigned-byte 4) "Iu4" :implementation-specific t :value #.(1- (expt 2 4)))
  ((unsigned-byte 8) "Iu8" :implementation-specific t :value #.(1- (expt 2 8)))
  ((unsigned-byte 16) "Iu16" :implementation-specific t :value #.(1- (expt 2 16)))
  ((unsigned-byte 32) "Iu32" :implementation-specific t :value #.(1- (expt 2 32)))
  ((unsigned-byte 64) "Iu64" :implementation-specific t :value #.(1- (expt 2 64)))
  ((unsigned-byte 128) "Iu128" :implementation-specific t :value #.(1- (expt 2 128)))

  ((signed-byte 2) "Is2" :implementation-specific t :value #.(- (expt 2 1)))
  ((signed-byte 4) "Is4" :implementation-specific t :value #.(- (expt 2 3)))
  ((signed-byte 8) "Is8" :implementation-specific t :value #.(- (expt 2 7)))
  ((signed-byte 16) "Is16" :implementation-specific t :value #.(- (expt 2 15)))
  ((signed-byte 32) "Is32" :implementation-specific t :value #.(- (expt 2 31)))
  ((signed-byte 64) "Is64" :implementation-specific t :value #.(- (expt 2 63)))
  ((signed-byte 128) "Is128" :implementation-specific t :value #.(- (expt 2 127)))

  (float "F")
  (short-float "Fsh" :implementation-specific t :value 1s0)
  (single-float "Fs" :implementation-specific t :value 1f0)
  (double-float "Fd" :implementation-specific t :value 1d0)
  (long-float "Fl" :implementation-specific t :value 1l0))

;; Complex Numbers
(define-type-name-pairs
  (complex "C")

  ;; This doesn't make sense since the real and imaginary components
  ;; of the complex number can only be of type real.
  #- (and)
  ((complex real) "CR" :implementation-specific t)
  ((complex rational) "CRa" :implementation-specific t)
  ((complex fixnum) "CIf" :implementation-specific t)

  ;; An implementation may choose to implement the type (complex
  ;; float) as (or (complex single-float) ...) which can lead to
  ;; situations like the following:
  ;;
  ;; CL-USER> (upgraded-complex-part-type 'float)
  ;; => real
  ;;
  ;; CL-USER> (subtypep '(complex real) '(complex float))
  ;; => nil
  ;; t
  ;;
  ;; CL-USER> (subtypep '(complex float) '(complex real))
  ;; => t
  ;; t
  ;;
  ;; Additionally, the standard says that the real and imaginary parts
  ;; of a complex number are floats, then the floats must be the same
  ;; type. This means that you can't create an object of type (complex
  ;; float) which is also not a member of the sets (complex
  ;; short-float), (complex single-float), (complex double-float) or
  ;; (complex long-float).
  ((complex float) "CF" :implementation-specific t)
  ((complex short-float) "Csh" :implementation-specific t)
  ((complex single-float) "Cs" :implementation-specific t)
  ((complex double-float) "Cd" :implementation-specific t)
  ((complex long-float) "Cl" :implementation-specific t)

  ((complex bit) "Cb" :implementation-specific t)
  ((complex (unsigned-byte 2)) "Cu2" :implementation-specific t)
  ((complex (unsigned-byte 4)) "Cu4" :implementation-specific t)
  ((complex (unsigned-byte 8)) "Cu8" :implementation-specific t)
  ((complex (unsigned-byte 16)) "Cu16" :implementation-specific t)
  ((complex (unsigned-byte 32)) "Cu32" :implementation-specific t)
  ((complex (unsigned-byte 64)) "Cu64" :implementation-specific t)
  ((complex (unsigned-byte 128)) "Cu128" :implementation-specific t)

  ((complex (signed-byte 2)) "Cs2" :implementation-specific t)
  ((complex (signed-byte 4)) "Cs4" :implementation-specific t)
  ((complex (signed-byte 8)) "Cs8" :implementation-specific t)
  ((complex (signed-byte 16)) "Cs16" :implementation-specific t)
  ((complex (signed-byte 32)) "Cs32" :implementation-specific t)
  ((complex (signed-byte 64)) "Cs64" :implementation-specific t)
  ((complex (signed-byte 128)) "Cs128" :implementation-specific t))

;; Arrays
(define-type-name-pairs
  (array "A")

  ((array float) "AF" :implementation-specific t)
  ((array short-float) "Ash" :implementation-specific t)
  ((array single-float) "As" :implementation-specific t)
  ((array double-float) "Ad" :implementation-specific t)
  ((array long-float) "Al" :implementation-specific t)

  ((array (unsigned-byte 1)) "Ab" :implementation-specific t)
  ((array (unsigned-byte 2)) "Au2" :implementation-specific t)
  ((array (unsigned-byte 4)) "Au4" :implementation-specific t)
  ((array (unsigned-byte 8)) "Au8" :implementation-specific t)
  ((array (unsigned-byte 16)) "Au16" :implementation-specific t)
  ((array (unsigned-byte 32)) "Au32" :implementation-specific t)
  ((array (unsigned-byte 64)) "Au64" :implementation-specific t)
  ((array (unsigned-byte 128)) "Au128" :implementation-specific t)

  ((array (signed-byte 2)) "As2" :implementation-specific t)
  ((array (signed-byte 4)) "As4" :implementation-specific t)
  ((array (signed-byte 8)) "As8" :implementation-specific t)
  ((array (signed-byte 16)) "As16" :implementation-specific t)
  ((array (signed-byte 32)) "As32" :implementation-specific t)
  ((array (signed-byte 64)) "As64" :implementation-specific t)
  ((array (signed-byte 128)) "As128" :implementation-specific t))

;; Simple Arrays
(define-type-name-pairs
  (simple-array "V")

  ((simple-array float) "VF" :implementation-specific t)
  ((simple-array short-float) "Vsh" :implementation-specific t)
  ((simple-array single-float) "Vs" :implementation-specific t)
  ((simple-array double-float) "Vd" :implementation-specific t)
  ((simple-array long-float) "Vl" :implementation-specific t)

  ((simple-array (unsigned-byte 1)) "Vb" :implementation-specific t)
  ((simple-array (unsigned-byte 2)) "Vu2" :implementation-specific t)
  ((simple-array (unsigned-byte 4)) "Vu4" :implementation-specific t)
  ((simple-array (unsigned-byte 8)) "Vu8" :implementation-specific t)
  ((simple-array (unsigned-byte 16)) "Vu16" :implementation-specific t)
  ((simple-array (unsigned-byte 32)) "Vu32" :implementation-specific t)
  ((simple-array (unsigned-byte 64)) "Vu64" :implementation-specific t)
  ((simple-array (unsigned-byte 128)) "Vu128" :implementation-specific t)

  ((simple-array (signed-byte 2)) "Vs2" :implementation-specific t)
  ((simple-array (signed-byte 4)) "Vs4" :implementation-specific t)
  ((simple-array (signed-byte 8)) "Vs8" :implementation-specific t)
  ((simple-array (signed-byte 16)) "Vs16" :implementation-specific t)
  ((simple-array (signed-byte 32)) "Vs32" :implementation-specific t)
  ((simple-array (signed-byte 64)) "Vs64" :implementation-specific t)
  ((simple-array (signed-byte 128)) "Vs128" :implementation-specific t))
