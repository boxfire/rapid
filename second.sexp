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


(defun prim__add_Int (v0 v1) ((START) (OP RVAL +Int v0 v1)))

