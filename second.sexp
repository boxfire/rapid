(defun Prelude.==_Eq__Int
  (v0 v1)
  ((DECLARE v2)
   (START)
   (OP v2 ==Int v0 v1)
   (CONSTCASE v2
     (default ((MKCON RVAL 0 ())))
     (0       ((MKCON RVAL 1 ()))))))


(defun Tiny.main
  ()
  ((DECLARE v1)
   (DECLARE v2)
   (DECLARE v0)
   (DECLARE v3)
   (START)
   (MKCONSTANT v1 8)
   (MKCONSTANT v2 12)
   (MKCONSTANT RVAL 12)
   (CALL v0 False Tiny.average (v1 v2))
   (MKCON v3 0 ())
   (CALL RVAL True Prelude.pure_Applicative__IO (DISCARD v3))))


(defun Prelude.div_Integral__Int
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (START)
   (MKCONSTANT v2 0)
   (CALL v3 False Prelude.fromInteger_Num__Int (v2))
   (CALL v4 False Prelude.==_Eq__Int (v1 v3))
   (CALL RVAL True "Prelude.case block in 2452(2573)" (v1 v0 v4))))

(defun "Prelude.case block in 2452(2573)"
  (v0 v1 v2)
  ((START) (CASE v2 (nodefault) (1 ((OP RVAL /Int v1 v0))))))

(defun Tiny.average
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (START)
   (CALL v2 False Prelude.+_Num__Int (v0 v1))
   (MKCONSTANT v3 2)
   (CALL RVAL True Prelude.div_Integral__Int (v2 v3))))


(defun prim__add_Int (v0 v1) ((START) (OP RVAL +Int v0 v1)))

