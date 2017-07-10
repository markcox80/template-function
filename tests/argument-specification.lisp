(in-package "TEMPLATE-FUNCTION.ARGUMENT-SPECIFICATION.TESTS")
(in-suite all-argument-specification-tests)

;;;; Parse lambda list tests

(test parse-lambda-list/whole
  ;; Valid
  (labels ((do-trial (lambda-list expected)
             (let* ((parameters (parse-lambda-list lambda-list))
                    (whole (whole-parameter parameters)))
               (if expected
                   (is-true (and (parameterp whole)
                                 (whole-parameter-p whole)
                                 (equal expected (parameter-var whole))
                                 (equalp (list whole) (remove-if-not #'whole-parameter-p (all-parameters parameters))))
                            "Invalid whole constituent after parsing lambda list ~A." lambda-list)
                   (is-true (and (null whole)
                                 (= 0 (count-if #'whole-parameter-p (all-parameters parameters))))
                            "Unexpected whole constituent after parsing lambda list ~A." lambda-list)))))
    (loop
      for whole in '(nil (&whole whole) (&whole whole2))
      for expected in '(nil whole whole2)
      do
         (dolist (required '(nil (a) (a b) (a b c)))
           (dolist (optional '(nil (&optional opt-1)))
             (dolist (others '(nil (&others others) (&others others2)))
               (dolist (rest '(nil (&rest args) (&rest args2)))
                 (when (or (and (null others) (null rest))
                           (and others rest))
                   (dolist (keys '(nil (&key) (&key &allow-other-keys) (&key x y) (&key z &allow-other-keys)))
                     (unless (and others keys)
                       (do-trial (append whole required optional others rest keys)
                         expected))))))))))

  ;; Errors
  (macrolet ((trial (lambda-list)
               `(signals parse-lambda-list-error (parse-lambda-list ',lambda-list))))
    (trial (&whole))
    (trial (&whole &others))
    (trial (&whole &rest))
    (trial (&whole &key))
    (trial (&whole w &whole x))
    (trial (a &whole w))
    (trial (&others a &whole w))
    (trial (&rest a &whole w))
    (trial (&key a &whole w))
    (trial (&key a &whole w &allow-other-keys))
    (trial (&key a &allow-other-keys &whole w))))

(test parse-lambda-list/required
  ;; Valid
  (labels ((do-trial (lambda-list expected)
             (let* ((parameters (parse-lambda-list lambda-list))
                    (required (required-parameters parameters)))
               (is-true (and (= (length expected) (length required))
                             (equalp required (remove-if-not #'required-parameter-p (all-parameters parameters)))
                             (loop
                               for e in expected
                               for p in required
                               always (eql e (parameter-var p))))
                        "Required parameters ~A found in ~A do not match ~A." required lambda-list expected))))
    (dolist (whole '(nil (&whole whole)))
      (loop
        for required in '(nil (a) (a b) (a b c))
        for expected in '(nil (a) (a b) (a b c))
        do
           (dolist (optional '(nil (&optional opt1)))
             (dolist (others '(nil (&others others) (&others others2)))
               (dolist (rest '(nil (&rest args) (&rest args2)))
                 (when (or (and (null others) (null rest))
                           (and others rest))
                   (dolist (keys '(nil (&key) (&key &allow-other-keys) (&key x y) (&key z &allow-other-keys)))
                     (unless (and others keys)
                       (do-trial (append whole required optional others rest keys)
                         expected))))))))))

  ;; Errors
  (macrolet ((trial (lambda-list)
               `(signals parse-lambda-list-error (parse-lambda-list ',lambda-list))))
    (trial (a b &others others c))
    (trial (a b &rest args c))
    (trial (a b &key &allow-other-keys c))))

(test parse-lambda-list/optional
  ;; Valid
  (labels ((do-trial (lambda-list expected)
             (let* ((parameters (parse-lambda-list lambda-list))
                    (optional (optional-parameters parameters)))
               (is-true (and (= (length expected) (length optional))
                             (equalp optional (remove-if-not #'optional-parameter-p (all-parameters parameters)))
                             (loop
                               for (e-var e-init-form e-varp) in expected
                               for p in optional
                               always (and (eql e-var (parameter-var p))
                                           (eql e-init-form (parameter-init-form p))
                                           (eql e-varp (parameter-varp p)))))
                        "Optional parameters ~A found in ~A do not match ~A." optional lambda-list expected))))
    (dolist (whole '(nil (&whole whole)))
      (dolist (required '(nil (a) (a b)))
        (loop
          for optional in '(nil (&optional opt-1 (opt-2)) (&optional (opt-1 cons opt-1-p) (opt-2 bit)))
          for expected in '(nil
                            ((opt-1 t nil) (opt-2 t nil))
                            ((opt-1 cons opt-1-p) (opt-2 bit)))
          do
             (dolist (others '(nil (&others others) (&others others2)))
               (dolist (rest '(nil (&rest args) (&rest args2)))
                 (when (or (and others rest)
                           (and (null others) (null rest)))
                   (dolist (keys '(nil (&key) (&key &allow-other-keys) (&key x y) (&key z &allow-other-keys)))
                     (unless (and others keys)
                       (do-trial (append whole required optional others rest keys)
                         expected))))))))))

  ;; Errors
  (macrolet ((trial (lambda-list)
               `(signals (parse-lambda-list-error "Failed to signal parse-lambda-list-error for lambda list ~A."
                                                  (',lambda-list))
                  (parse-lambda-list ',lambda-list))))
    (trial (&optional opt-1 &optional opt-2))
    (trial (&others others2 &optional opt-1))
    (trial (&optional &optional))
    (trial (a &rest args &optional b))
    (trial (a &key b &optional (c 'bit cp)))))

(test parse-lambda-list/others
  ;; Valid
  (labels ((do-trial (lambda-list expected)
             (let* ((parameters (parse-lambda-list lambda-list))
                    (others (others-parameter parameters)))
               (if expected
                   (is-true (and (others-parameter-p others)
                                 (equalp (list others) (remove-if-not #'others-parameter-p (all-parameters parameters)))
                                 (eql expected (parameter-var others)))
                            "Invalid others constituent after parsing lambda list ~A." lambda-list)
                   (is-true (and (null others)
                                 (= 0 (count-if #'others-parameter-p (all-parameters parameters))))
                            "Unexpected others constituent after parsing lambda list ~A." lambda-list)))))
    (dolist (whole '(nil (&whole whole)))
      (dolist (required '(nil (a) (a b)))
        (dolist (optional '(nil (&optional (opt-1 bit opt-1-p) opt-2)))
          (loop
            for others in '((&others others) (&others others2))
            for expected in '(others others2)
            do
               (dolist (rest '((&rest args)))
                 (do-trial (append whole required optional others rest)
                   expected)))))))

  ;; Errors
  (macrolet ((trial (lambda-list)
               `(signals parse-lambda-list-error (parse-lambda-list ',lambda-list))))
    (trial (&others))
    (trial (a &others))
    (trial (&others &rest))
    (trial (a &others &rest))
    (trial (&others &key))
    (trial (a &others &key))
    (trial (&others others &others others2))
    (trial (&rest args &others others))
    (trial (&key a &others others))
    (trial (&key a &allow-other-keys &others others))
    (trial (&others others &key a))
    (trial (a b &others others))
    (trial (a &rest args))))

(test parse-lambda-list/rest
  ;; Valid
  (labels ((do-trial (lambda-list expected)
             (let* ((parameters (parse-lambda-list lambda-list))
                    (rest (rest-parameter parameters)))
               (if expected
                   (is-true (and (rest-parameter-p rest)
                                 (equalp (list rest) (remove-if-not #'rest-parameter-p (all-parameters parameters)))
                                 (eql expected (parameter-var rest)))
                            "Invalid rest constituent after parsing lambda list ~A." lambda-list)
                   (is-true (and (null rest)
                                 (= 0 (count-if #'rest-parameter-p (all-parameters parameters))))
                            "Unexpected rest constituent after parsing lambda list ~A." lambda-list)))))
    (dolist (whole '(nil (&whole whole)))
      (dolist (required '(nil (a b)))
        (dolist (optional '(nil (&optional opt-1)))
          (dolist (others '((&others others)))
            (loop
              for rest in '((&rest args) (&rest args2))
              for expected in '(args args2)
              do
                 (dolist (keys '(nil (&key) (&key &allow-other-keys) (&key w x) (&key z &allow-other-keys)))
                   (unless (and others keys)
                     (do-trial (append whole required optional others rest keys)
                       expected)))))))))

  ;; Errors
  (macrolet ((trial (lambda-list)
               `(signals parse-lambda-list-error (parse-lambda-list ',lambda-list))))
    (trial (&rest))
    (trial (&whole w &rest))
    (trial (a &rest))
    (trial (&whole w a &rest))
    (trial (&optional opt-1 &rest))
    (trial (&rest args &optional a))
    (trial (&rest args &rest))
    (trial (&rest args &rest args2))
    (trial (&key a &rest args))
    (trial (&rest args &others others))
    (trial (&rest args a))
    (trial (&rest args &whole w))
    (trial (a b &others others &rest args &key (d t)))
    (trial (a b &others others))))

(test parse-lambda-list/key
  ;; Valid
  (labels ((do-trial (lambda-list expected)
             (let* ((parameters (parse-lambda-list lambda-list))
                    (keys (keyword-parameters parameters))
                    (allow-other-keys-p (and (find '&allow-other-keys lambda-list)
                                             t))
                    (keys? (and (find '&key lambda-list)
                                t)))
               (is-true (and (= (length expected) (length keys))
                             (eql keys? (keyword-parameters-p parameters))
                             (eql allow-other-keys-p (allow-other-keywords-p parameters))
                             (every #'keyword-parameter-p keys)
                             (loop
                               for (keyword var init-form varp) in expected
                               for key in keys
                               always
                               (and (eql keyword (parameter-keyword key))
                                    (eql var (parameter-var key))
                                    (equalp init-form (parameter-init-form key))
                                    (eql varp (parameter-varp key)))))
                        "Invalid keyword constituents ~A found in lambda list ~A." keys lambda-list))))
    (dolist (whole '(nil (&whole whole)))
      (dolist (required '(nil (a b)))
        (dolist (optional '(nil (&optional opt-1)))
          (dolist (rest '(nil (&rest args)))
            (loop
              for keys in '((&key) (&key &allow-other-keys) (&key x ((:y z) nil zp) (w hey) ((:g g))))
              for expected in '(nil nil ((:x x t nil) (:y z nil zp) (:w w hey nil) (:g g t nil)))
              do
                 (do-trial (append whole required optional rest keys)
                   expected)))))))

  ;; Error
  (macrolet ((trial (lambda-list)
               `(signals parse-lambda-list-error (parse-lambda-list ',lambda-list))))
    (trial (&key &key))
    (trial (&key &allow-other-keys a))
    (trial (&key &allow-other-keys &allow-other-keys))
    (trial (&others others &key &rest args))
    (trial (&key &whole w))
    (trial (&key &whole w &allow-other-keys))
    (trial (&key &rest args))
    (trial (&key &optional opt-1))))

(test parse-lambda-list/duplicate-vars
  (macrolet ((trial (lambda-list)
               `(signals duplicate-variable-error (parse-lambda-list ',lambda-list))))
    (trial (&whole whole whole))
    (trial (&whole whole &others whole &rest args))
    (trial (&whole whole &others others &rest whole))
    (trial (&whole whole &key whole))
    (trial (&whole whole &key ((:foo whole))))
    (trial (&whole whole &key (foo nil whole)))
    (trial (a &optional a))
    (trial (a &optional (b nil a)))
    (trial (a &others b &rest a))
    (trial (a &key a))
    (trial (a &key (b nil a)))
    (trial (a &key ((:a b) nil a)))
    (trial (a &others a &rest b))
    (trial (&others a &rest a))
    (trial (&optional a &key a))))

;;;; argument-specification-lambda

(test argument-specification-lambda/whole
  (let* ((fn (argument-specification-lambda (&whole whole a)
               (declare (ignore a))
               whole)))
    (is (equal '(t) (funcall fn '(t))))
    (is (equal '(integer) (funcall fn '(integer))))

    (signals argument-specification-lambda-error (funcall fn '(integer t)))
    (signals argument-specification-lambda-error (funcall fn '(&rest t)))
    (signals argument-specification-lambda-error (funcall fn '(&key)))
    (signals argument-specification-lambda-error (funcall fn '(&key (:a t))))))

(test argument-specification-lambda/required
  (let* ((fn (argument-specification-lambda (a b c)
               (list a b c))))
    (macrolet ((trial (expected input)
                 `(is (equal ',expected (funcall fn ',input)))))
      (trial (t t t) (t t t))
      (trial (bit t t) (bit t t))
      (trial (t bit t) (t bit t))
      (trial (t t bit) (t t bit)))

    (macrolet ((trial (&rest arg-spec)
                 `(signals argument-specification-lambda-error
                    (funcall fn ',arg-spec))))
      (trial)
      (trial t)
      (trial t t)
      (trial t t t t)
      (trial &rest t)
      (trial t &rest t)
      (trial t t &rest t)
      (trial t t t &rest t)
      (trial &key (a t))
      (trial t &key (a t))
      (trial t t &key (a t))
      (trial t t t &key (a t)))))

(test argument-specification-lambda/rest-and-others
  (let* ((fn (argument-specification-lambda (a &others others &rest args)
               (list a others args))))
    (macrolet ((trial (expected input)
                 `(is (equal ',expected (funcall fn ',input)))))
      (trial (t nil t) (t &rest t))
      (trial (t nil integer) (t &rest integer))
      (trial (bit nil integer) (bit &rest integer))
      (trial (t (t) t) (t t &rest t))
      (trial (bit (integer bit) float) (bit integer bit &rest float)))

    (macrolet ((trial (&rest arg-spec)
                 `(signals argument-specification-lambda-error
                    (funcall fn ',arg-spec))))
      (trial t)
      (trial &rest integer)
      (trial &key (a bit))
      (trial &optional bit)
      (trial t &key (a bit))
      (trial t &rest integer &rest bit)
      (trial t &rest)))

  ;; Whole
  (let* ((fn (argument-specification-lambda (&whole whole a &others others &rest args)
               (list whole a others args)))
         (act (funcall fn '(t t t &rest t))))
    (is (equal '((t t t &rest t) t (t t) t)
               act))))

(test argument-specification-lambda/rest-sans-others
  (let* ((fn (argument-specification-lambda (a b &rest args)
               (list a b args))))
    (macrolet ((trial (expected input)
                 `(is (equal ',expected (funcall fn ',input)))))
      (trial (t t t) (t t &rest t))
      (trial (t bit integer) (t bit &rest integer))
      (trial (float bit string) (float bit &rest string)))

    (macrolet ((trial (&rest arg-spec)
                 `(signals argument-specification-lambda-error
                    (funcall fn ',arg-spec))))
      (trial)
      (trial t t)
      (trial &rest t)
      (trial t &rest t)
      (trial t t &rest t &rest t)
      (trial &rest t &rest t)
      (trial t t t)))

  ;; Whole
  (let* ((fn (argument-specification-lambda (&whole whole a &rest args)
               (list whole a args)))
         (act (funcall fn '(t &rest t))))
    (is (equal '((t &rest t) t t)
               act))))

(test argument-specification-lambda/keys-sans-allow-others
  (let* ((fn (argument-specification-lambda (a b &key (c nil cp) ((:d foo)) e)
               (list a b (list c cp) foo e))))
    (labels ((do-trial (expected input)
               (is (equal expected (funcall fn input)))))
      (macrolet ((trial (expected input)
                   `(do-trial ',expected ',input)))
        (trial (t t (nil nil) t t)
               (t t))
        (trial (t t (nil nil) t t)
               (t t &key))
        (trial (bit integer (float t) t t)
               (bit integer &key (:c float)))
        (trial (integer bit (nil nil) float t)
               (integer bit &key (:d float)))
        (trial (float string (nil nil) t character)
               (float string &key (:e character)))
        (trial (t t (string t) character bit)
               (t t &key (:c string) (:d character) (:e bit)))))

    (macrolet ((trial (&rest arg-spec)
                 `(signals argument-specification-lambda-error (funcall fn ',arg-spec))))
      (trial)
      (trial t)
      (trial t t t)
      (trial t t (:hey string))
      (trial t t (:hey))
      (trial t t (:c string) (:hey 1 2 3))))

  ;; Whole
  (let* ((fn (argument-specification-lambda (&whole whole a &key b)
               (list whole a b)))
         (act (funcall fn '(t &key (:b t)))))
    (is (equal '((t &key (:b t)) t t)
               act))))

(test argument-specification-lambda/keys-and-allow-others
  (let* ((fn (argument-specification-lambda (a b &key (c nil cp) ((:d foo)) e &allow-other-keys)
               (list a b (list c cp) foo e))))
    (labels ((do-trial (expected input)
               (is (equal expected (funcall fn input)))))
      (macrolet ((trial (expected input)
                   `(do-trial ',expected ',input)))
        (trial (t t (nil nil) t t)
               (t t &key (:hey string)))
        (trial (t t (nil nil) t t)
               (t t &key (:hey string)))
        (trial (bit integer (float t) t t)
               (bit integer &key (:c float) (:hey string)))
        (trial (integer bit (nil nil) float t)
               (integer bit &key (:d float) (:hey string)))
        (trial (float string (nil nil) t character)
               (float string &key (:e character) (:hey string)))
        (trial (t t (string t) character bit)
               (t t &key (:c string) (:d character) (:hey string) (:e bit)))))

    (macrolet ((trial (&rest arg-spec)
                 `(signals argument-specification-lambda-error (funcall fn ',arg-spec))))
      (trial)
      (trial t)
      (trial t t t)
      (trial t t (:hey))
      (trial t t (:c string) (:hey 1 2 3))))

  ;; Whole
  (let* ((fn (argument-specification-lambda (&whole whole a &key b &allow-other-keys)
               (list whole a b)))
         (act (funcall fn '(t &key (:b t) (:hey string)))))
    (is (equal '((t &key (:b t) (:hey string)) t t)
               act))))

(test argument-specification-lambda/declarations
  ;; Required
  (let* ((fn (argument-specification-lambda (&whole whole a b)
               (declare (ignore whole a b))
               nil)))
    (finishes (funcall fn '(t t))))

  ;; Others and rest
  (let* ((fn (argument-specification-lambda (&whole whole a b &others others &rest args)
               (declare (ignore whole a b others args))
               nil)))
    (finishes (funcall fn '(t t t t &rest t))))

  ;; Others sans rest
  (let* ((fn (argument-specification-lambda (&whole whole a b &rest args)
               (declare (ignore whole a b args))
               nil)))
    (finishes (funcall fn '(t t &rest t))))

  ;; Keywords
  (let* ((fn (argument-specification-lambda (&whole whole a b &key (c nil cp) ((:d foo)) e)
               (declare (ignore whole a b c cp foo e))
               nil)))
    (finishes (funcall fn '(t t)))))

(test argument-specification-lambda/init-forms
  (let* ((fn (argument-specification-lambda (&whole whole a b &key (c (list a b) cp) (d (list whole c cp)))
               (list c d)))
         (act (funcall fn '(bit integer))))
    (is (equal '(bit integer) (first act)))
    (is (equal '((bit integer) (bit integer) nil) (second act)))))
