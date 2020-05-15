(defun {__mainExpression:0}
  ()
  ((DECLARE v0)
   (START)
   (CALL v0 False Tiny.main ())
   (CALL RVAL True PrimIO.unsafePerformIO (DISCARD v0))))

(defun prim__add_Int (v0 v1) ((START) (OP RVAL +Int v0 v1)))

(defun prim__add_Integer (v0 v1) ((START) (OP RVAL +Integer v0 v1)))

(defun prim__sub_Integer (v0 v1) ((START) (OP RVAL -Integer v0 v1)))

(defun prim__mul_Int (v0 v1) ((START) (OP RVAL *Int v0 v1)))

(defun prim__mul_Integer (v0 v1) ((START) (OP RVAL *Integer v0 v1)))

(defun prim__div_Int (v0 v1) ((START) (OP RVAL /Int v0 v1)))

(defun prim__div_Integer (v0 v1) ((START) (OP RVAL /Integer v0 v1)))

(defun prim__mod_Int (v0 v1) ((START) (OP RVAL %Int v0 v1)))

(defun prim__mod_Integer (v0 v1) ((START) (OP RVAL %Integer v0 v1)))

(defun prim__lt_Int (v0 v1) ((START) (OP RVAL <Int v0 v1)))

(defun prim__lte_Int (v0 v1) ((START) (OP RVAL <=Int v0 v1)))

(defun prim__lte_Integer (v0 v1) ((START) (OP RVAL <=Integer v0 v1)))

(defun prim__eq_Int (v0 v1) ((START) (OP RVAL ==Int v0 v1)))

(defun prim__eq_Integer (v0 v1) ((START) (OP RVAL ==Integer v0 v1)))

(defun prim__gte_Int (v0 v1) ((START) (OP RVAL >=Int v0 v1)))

(defun prim__gt_Int (v0 v1) ((START) (OP RVAL >Int v0 v1)))

(defun prim__cast_IntegerInt (v0) ((START) (OP RVAL cast-Integer-Int v0)))

(defun "Tiny.case block in 1164(260)"
  (v0 v1)
  ((START)
   (CASE v1
     (nodefault)
     (0 ((MKCON RVAL 1 ())))
     (1 ((MKCON RVAL 0 ()))))))

(defun Tiny.main
  ()
  ((DECLARE v1)
   (DECLARE v2)
   (DECLARE v0)
   (DECLARE v3)
   (START)
   (MKCONSTANT v1 8)
   (MKCONSTANT v2 12)
   (CALL v0 False Tiny.average (v1 v2))
   (MKCON v3 0 ())
   (CALL RVAL True Prelude.pure_Applicative__IO (DISCARD v3))))

(defun Tiny.getres
  (v0)
  ((DECLARE v1)
   (DECLARE v2)
   (DECLARE v3)
   (START)
   (MKCONSTANT v1 10)
   (CALL v2 False Prelude.fromInteger_Num__Int (v1))
   (CALL v3   False Prelude.>_Ord__Int             (v0 v2))
   (CALL RVAL True  "Tiny.case block in 1164(260)" (v0 v3))))

(defun Tiny.complicatedFun
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v5)
   (DECLARE v4)
   (DECLARE v7)
   (DECLARE v6)
   (DECLARE v9)
   (DECLARE v10)
   (DECLARE v8)
   (DECLARE v11)
   (DECLARE v13)
   (DECLARE v14)
   (DECLARE v12)
   (DECLARE v15)
   (START)
   (CALL v2 False Prelude.+_Num__Int (v0 v1))
   (CALL v3 False Prelude.*_Num__Int (v0 v1))
   (MKCONSTANT v5 2)
   (CALL v4 False Prelude.+_Num__Int (v0 v5))
   (MKCONSTANT v7 2)
   (CALL v6  False Prelude.+_Num__Int (v1  v7))
   (CALL v9  False Prelude.*_Num__Int (v6  v0))
   (CALL v10 False Prelude.+_Num__Int (v4  v9))
   (CALL v8  False Prelude.+_Num__Int (v10 v1))
   (CALL v11 False Prelude.+_Num__Int (v2  v3))
   (CALL v13 False Prelude.+_Num__Int (v2  v3))
   (CALL v14 False Prelude.+_Num__Int (v13 v11))
   (CALL v12 False Prelude.+_Num__Int (v14 v8))
   (CALL v15 False Prelude.+_Num__Int (v12 v0))
   (ASSIGN RVAL v15)))

(defun Tiny.averageb
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (START)
   (CALL v2 False Prelude.+_Num__Integer (v0 v1))
   (MKCONSTANT v3 2)
   (CALL RVAL True Prelude.div_Integral__Integer (v2 v3))))

(defun Tiny.average
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (START)
   (CALL v2 False Prelude.+_Num__Int (v0 v1))
   (MKCONSTANT v3 2)
   (CALL RVAL True Prelude.div_Integral__Int (v2 v3))))

(defun "Prelude.case block in 3412(3660)"
  (v0 v1)
  ((DECLARE v3)
   (DECLARE v2)
   (DECLARE v4)
   (START)
   (CASE v1
     (nodefault)
     (0 ((MKCONSTANT RVAL 0)))
     (1 (
       (MKCONSTANT v3 1)
       (MKCONSTANT v2 1)
       (OP v4   -Integer v0 v2)
       (OP RVAL +Integer v3 v4))))))

(defun "Prelude.case block in 2453(2587)"
  (v0 v1 v2)
  ((START) (CASE v2 (nodefault) (1 ((OP RVAL %Int v1 v0))))))

(defun "Prelude.case block in 2452(2573)"
  (v0 v1 v2)
  ((START) (CASE v2 (nodefault) (1 ((OP RVAL /Int v1 v0))))))

(defun "Prelude.case block in 2361(2499)"
  (v0 v1 v2)
  ((START) (CASE v2 (nodefault) (1 ((OP RVAL %Integer v1 v0))))))

(defun "Prelude.case block in 2360(2485)"
  (v0 v1 v2)
  ((START) (CASE v2 (nodefault) (1 ((OP RVAL /Integer v1 v0))))))

(defun "Prelude.case block in 1277(1476)"
  (v0 v1 v2)
  ((START)
   (CASE v2
     (nodefault)
     (0 ((MKCON RVAL 1 ())))
     (1 ((MKCON RVAL 2 ()))))))

(defun "Prelude.case block in 1212(1459)"
  (v0 v1 v2)
  ((DECLARE v3)
   (START)
   (CASE v2
     (nodefault)
     (0 ((MKCON RVAL 0 ())))
     (1 (
       (CALL v3 False Prelude.==_Eq__Int (v1 v0))
       (CALL RVAL True "Prelude.case block in 1277(1476)" (v0 v1 v3)))))))

(defun "Prelude.case block in 1217(1442)"
  (v0 v1 v2)
  ((START)
   (CASE v2
     (nodefault)
     (0 ((ASSIGN RVAL v1)))
     (1 ((ASSIGN RVAL v0))))))

(defun "Prelude.case block in 1218(1428)"
  (v0 v1 v2)
  ((START)
   (CASE v2
     (nodefault)
     (0 ((ASSIGN RVAL v1)))
     (1 ((ASSIGN RVAL v0))))))

(defun Prelude.pure_Applicative__IO
  (v0 v1)
  ((START) (MKCLOSURE RVAL Prelude.{pure_Applicative__IO:0} 1 (v0 v1))))

(defun Prelude.{pure_Applicative__IO:0}
  (v0 v1 v2)
  ((START) (CALL RVAL True PrimIO.io_pure (DISCARD v1 v2))))

(defun Prelude.mod_Integral__Integer
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (START)
   (MKCONSTANT v2 0)
   (CALL v3 False Prelude.fromInteger_Num__Integer (v2))
   (CALL v4 False Prelude.==_Eq__Integer (v1 v3))
   (CALL RVAL True "Prelude.case block in 2361(2499)" (v1 v0 v4))))

(defun Prelude.mod_Integral__Int
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (START)
   (MKCONSTANT v2 0)
   (CALL v3 False Prelude.fromInteger_Num__Int (v2))
   (CALL v4 False Prelude.==_Eq__Int (v1 v3))
   (CALL RVAL True "Prelude.case block in 2453(2587)" (v1 v0 v4))))

(defun Prelude.min_Ord__Int
  (v0 v1)
  ((DECLARE v2)
   (START)
   (CALL v2 False Prelude.<_Ord__Int (v0 v1))
   (CALL RVAL True "Prelude.case block in 1218(1428)" (v1 v0 v2))))

(defun Prelude.max_Ord__Int
  (v0 v1)
  ((DECLARE v2)
   (START)
   (CALL v2 False Prelude.>_Ord__Int (v0 v1))
   (CALL RVAL True "Prelude.case block in 1217(1442)" (v1 v0 v2))))

(defun Prelude.map_Functor__IO
  (v0 v1 v2 v3)
  ((DECLARE v4)
   (DECLARE v5)
   (DECLARE v6)
   (START)
   (ASSIGN v4 v3)
   (MKCLOSURE v5 Prelude.{map_Functor__IO:0} 2 (v4 v0 v1 v2 v3))
   (MKCLOSURE v6 Prelude.{map_Functor__IO:1} 2 (v0 v1 v2 v3))
   (APPLY RVAL v5 v6)))

(defun Prelude.{map_Functor__IO:1}
  (v0 v1 v2 v3 v5 v4)
  ((DECLARE v6)
   (START)
   (APPLY v6 v2 v5)
   (CALL RVAL True PrimIO.io_pure (DISCARD v6 v4))))

(defun Prelude.{map_Functor__IO:0}
  (v0 v1 v2 v3 v4 v6 v5)
  ((DECLARE v7)
   (START)
   (APPLY v7 v0 v5)
   (CALL
    RVAL
    True
    "PrimIO.case block in 181(302)"
    (DISCARD DISCARD DISCARD v6 DISCARD v7))))

(defun Prelude.fromInteger_Num__Integer (v0) ((START) (ASSIGN RVAL v0)))

(defun Prelude.fromInteger_Num__Int
  (v0)
  ((START) (OP RVAL cast-Integer-Int v0)))

(defun Prelude.div_Integral__Integer
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (START)
   (MKCONSTANT v2 0)
   (CALL v3 False Prelude.fromInteger_Num__Integer (v2))
   (CALL v4 False Prelude.==_Eq__Integer (v1 v3))
   (CALL RVAL True "Prelude.case block in 2360(2485)" (v1 v0 v4))))

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

(defun Prelude.compare_Ord__Int
  (v0 v1)
  ((DECLARE v2)
   (START)
   (CALL v2 False Prelude.<_Ord__Int (v0 v1))
   (CALL RVAL True "Prelude.case block in 1212(1459)" (v1 v0 v2))))

(defun Prelude.__Impl_Ord_Int
  ()
  ((DECLARE v0)
   (DECLARE v1)
   (DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (DECLARE v5)
   (DECLARE v6)
   (DECLARE v7)
   (DECLARE v8)
   (DECLARE v9)
   (START)
   (MKCLOSURE v0 Prelude.{__Impl_Ord_Int:0} 2 ())
   (MKCLOSURE v1 Prelude.{__Impl_Ord_Int:1} 2 ())
   (MKCON v2 0 (v0 v1))
   (MKCLOSURE v3 Prelude.{__Impl_Ord_Int:2} 2 ())
   (MKCLOSURE v4 Prelude.{__Impl_Ord_Int:3} 2 ())
   (MKCLOSURE v5 Prelude.{__Impl_Ord_Int:4} 2 ())
   (MKCLOSURE v6 Prelude.{__Impl_Ord_Int:5} 2 ())
   (MKCLOSURE v7 Prelude.{__Impl_Ord_Int:6} 2 ())
   (MKCLOSURE v8 Prelude.{__Impl_Ord_Int:7} 2 ())
   (MKCLOSURE v9 Prelude.{__Impl_Ord_Int:8} 2 ())
   (MKCON RVAL 0 (v2 v3 v4 v5 v6 v7 v8 v9))))

(defun Prelude.{__Impl_Ord_Int:8}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.min_Ord__Int (v1 v0))))

(defun Prelude.{__Impl_Ord_Int:7}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.max_Ord__Int (v1 v0))))

(defun Prelude.{__Impl_Ord_Int:6}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.>=_Ord__Int (v1 v0))))

(defun Prelude.{__Impl_Ord_Int:5}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.<=_Ord__Int (v1 v0))))

(defun Prelude.{__Impl_Ord_Int:4}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.>_Ord__Int (v1 v0))))

(defun Prelude.{__Impl_Ord_Int:3}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.<_Ord__Int (v1 v0))))

(defun Prelude.{__Impl_Ord_Int:2}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.compare_Ord__Int (v1 v0))))

(defun Prelude.{__Impl_Ord_Int:1}
  (v1 v0)
  ((START) (CALL RVAL True Prelude./=_Eq__Int (v1 v0))))

(defun Prelude.{__Impl_Ord_Int:0}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.==_Eq__Int (v1 v0))))

(defun Prelude.__Impl_Num_Integer
  ()
  ((DECLARE v0)
   (DECLARE v1)
   (DECLARE v2)
   (START)
   (MKCLOSURE v0 Prelude.{__Impl_Num_Integer:0} 2 ())
   (MKCLOSURE v1 Prelude.{__Impl_Num_Integer:1} 2 ())
   (MKCLOSURE v2 Prelude.{__Impl_Num_Integer:2} 1 ())
   (MKCON RVAL 0 (v0 v1 v2))))

(defun Prelude.{__Impl_Num_Integer:2}
  (v0)
  ((START) (CALL RVAL True Prelude.fromInteger_Num__Integer (v0))))

(defun Prelude.{__Impl_Num_Integer:1}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.*_Num__Integer (v1 v0))))

(defun Prelude.{__Impl_Num_Integer:0}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.+_Num__Integer (v1 v0))))

(defun Prelude.__Impl_Num_Int
  ()
  ((DECLARE v0)
   (DECLARE v1)
   (DECLARE v2)
   (START)
   (MKCLOSURE v0 Prelude.{__Impl_Num_Int:0} 2 ())
   (MKCLOSURE v1 Prelude.{__Impl_Num_Int:1} 2 ())
   (MKCLOSURE v2 Prelude.{__Impl_Num_Int:2} 1 ())
   (MKCON RVAL 0 (v0 v1 v2))))

(defun Prelude.{__Impl_Num_Int:2}
  (v0)
  ((START) (CALL RVAL True Prelude.fromInteger_Num__Int (v0))))

(defun Prelude.{__Impl_Num_Int:1}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.*_Num__Int (v1 v0))))

(defun Prelude.{__Impl_Num_Int:0}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.+_Num__Int (v1 v0))))

(defun Prelude.__Impl_Integral_Integer
  ()
  ((DECLARE v0)
   (DECLARE v1)
   (DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (DECLARE v5)
   (START)
   (MKCLOSURE v0 Prelude.{__Impl_Integral_Integer:0} 2 ())
   (MKCLOSURE v1 Prelude.{__Impl_Integral_Integer:1} 2 ())
   (MKCLOSURE v2 Prelude.{__Impl_Integral_Integer:2} 1 ())
   (MKCON v3 0 (v0 v1 v2))
   (MKCLOSURE v4 Prelude.{__Impl_Integral_Integer:3} 2 ())
   (MKCLOSURE v5 Prelude.{__Impl_Integral_Integer:4} 2 ())
   (MKCON RVAL 0 (v3 v4 v5))))

(defun Prelude.{__Impl_Integral_Integer:4}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.mod_Integral__Integer (v1 v0))))

(defun Prelude.{__Impl_Integral_Integer:3}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.div_Integral__Integer (v1 v0))))

(defun Prelude.{__Impl_Integral_Integer:2}
  (v0)
  ((START) (CALL RVAL True Prelude.fromInteger_Num__Integer (v0))))

(defun Prelude.{__Impl_Integral_Integer:1}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.*_Num__Integer (v1 v0))))

(defun Prelude.{__Impl_Integral_Integer:0}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.+_Num__Integer (v1 v0))))

(defun Prelude.__Impl_Integral_Int
  ()
  ((DECLARE v0)
   (DECLARE v1)
   (DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (DECLARE v5)
   (START)
   (MKCLOSURE v0 Prelude.{__Impl_Integral_Int:0} 2 ())
   (MKCLOSURE v1 Prelude.{__Impl_Integral_Int:1} 2 ())
   (MKCLOSURE v2 Prelude.{__Impl_Integral_Int:2} 1 ())
   (MKCON v3 0 (v0 v1 v2))
   (MKCLOSURE v4 Prelude.{__Impl_Integral_Int:3} 2 ())
   (MKCLOSURE v5 Prelude.{__Impl_Integral_Int:4} 2 ())
   (MKCON RVAL 0 (v3 v4 v5))))

(defun Prelude.{__Impl_Integral_Int:4}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.mod_Integral__Int (v1 v0))))

(defun Prelude.{__Impl_Integral_Int:3}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.div_Integral__Int (v1 v0))))

(defun Prelude.{__Impl_Integral_Int:2}
  (v0)
  ((START) (CALL RVAL True Prelude.fromInteger_Num__Int (v0))))

(defun Prelude.{__Impl_Integral_Int:1}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.*_Num__Int (v1 v0))))

(defun Prelude.{__Impl_Integral_Int:0}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.+_Num__Int (v1 v0))))

(defun Prelude.__Impl_Functor_IO
  (v0 v1 v2 v3)
  ((START) (CALL RVAL True Prelude.map_Functor__IO (DISCARD DISCARD v2 v3))))

(defun Prelude.__Impl_Eq_Integer
  ()
  ((DECLARE v0)
   (DECLARE v1)
   (START)
   (MKCLOSURE v0 Prelude.{__Impl_Eq_Integer:0} 2 ())
   (MKCLOSURE v1 Prelude.{__Impl_Eq_Integer:1} 2 ())
   (MKCON RVAL 0 (v0 v1))))

(defun Prelude.{__Impl_Eq_Integer:1}
  (v1 v0)
  ((START) (CALL RVAL True Prelude./=_Eq__Integer (v1 v0))))

(defun Prelude.{__Impl_Eq_Integer:0}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.==_Eq__Integer (v1 v0))))

(defun Prelude.__Impl_Eq_Int
  ()
  ((DECLARE v0)
   (DECLARE v1)
   (START)
   (MKCLOSURE v0 Prelude.{__Impl_Eq_Int:0} 2 ())
   (MKCLOSURE v1 Prelude.{__Impl_Eq_Int:1} 2 ())
   (MKCON RVAL 0 (v0 v1))))

(defun Prelude.{__Impl_Eq_Int:1}
  (v1 v0)
  ((START) (CALL RVAL True Prelude./=_Eq__Int (v1 v0))))

(defun Prelude.{__Impl_Eq_Int:0}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.==_Eq__Int (v1 v0))))

(defun Prelude.__Impl_Applicative_IO
  ()
  ((DECLARE v0)
   (DECLARE v1)
   (DECLARE v2)
   (START)
   (MKCLOSURE v0 Prelude.{__Impl_Applicative_IO:0} 4 ())
   (MKCLOSURE v1 Prelude.{__Impl_Applicative_IO:1} 2 ())
   (MKCLOSURE v2 Prelude.{__Impl_Applicative_IO:2} 4 ())
   (MKCON RVAL 0 (v0 v1 v2))))

(defun Prelude.{__Impl_Applicative_IO:2}
  (v3 v2 v1 v0)
  ((START)
   (CALL RVAL True Prelude.<*>_Applicative__IO (DISCARD DISCARD v1 v0))))

(defun Prelude.{__Impl_Applicative_IO:1}
  (v1 v0)
  ((START) (CALL RVAL True Prelude.pure_Applicative__IO (DISCARD v0))))

(defun Prelude.{__Impl_Applicative_IO:0}
  (v3 v2 v1 v0)
  ((START) (CALL RVAL True Prelude.map_Functor__IO (DISCARD DISCARD v1 v0))))

(defun Prelude.>_Ord__Int
  (v0 v1)
  ((DECLARE v2)
   (START)
   (OP v2 >Int v0 v1)
   (CONSTCASE v2
     (default ((MKCON RVAL 0 ())))
     (0       ((MKCON RVAL 1 ()))))))

(defun Prelude.>=_Ord__Int
  (v0 v1)
  ((DECLARE v2)
   (START)
   (OP v2 >=Int v0 v1)
   (CONSTCASE v2
     (default ((MKCON RVAL 0 ())))
     (0       ((MKCON RVAL 1 ()))))))

(defun Prelude.==_Eq__Integer
  (v0 v1)
  ((DECLARE v2)
   (START)
   (OP v2 ==Integer v0 v1)
   (CONSTCASE v2
     (default ((MKCON RVAL 0 ())))
     (0       ((MKCON RVAL 1 ()))))))

(defun Prelude.==_Eq__Int
  (v0 v1)
  ((DECLARE v2)
   (START)
   (OP v2 ==Int v0 v1)
   (CONSTCASE v2
     (default ((MKCON RVAL 0 ())))
     (0       ((MKCON RVAL 1 ()))))))

(defun Prelude.<_Ord__Int
  (v0 v1)
  ((DECLARE v2)
   (START)
   (OP v2 <Int v0 v1)
   (CONSTCASE v2
     (default ((MKCON RVAL 0 ())))
     (0       ((MKCON RVAL 1 ()))))))

(defun Prelude.<=_Ord__Int
  (v0 v1)
  ((DECLARE v2)
   (START)
   (OP v2 <=Int v0 v1)
   (CONSTCASE v2
     (default ((MKCON RVAL 0 ())))
     (0       ((MKCON RVAL 1 ()))))))

(defun Prelude.<*>_Applicative__IO
  (v0 v1 v2 v3)
  ((DECLARE v4)
   (DECLARE v5)
   (DECLARE v6)
   (START)
   (ASSIGN v4 v2)
   (MKCLOSURE v5 Prelude.{<*>_Applicative__IO:0} 2 (v4 v0 v1 v2 v3))
   (MKCLOSURE v6 Prelude.{<*>_Applicative__IO:3} 1 (v0 v1 v2 v3))
   (APPLY RVAL v5 v6)))

(defun Prelude.{<*>_Applicative__IO:3}
  (v0 v1 v2 v3 v4)
  ((DECLARE v5)
   (DECLARE v6)
   (DECLARE v7)
   (START)
   (ASSIGN v5 v3)
   (MKCLOSURE v6 Prelude.{<*>_Applicative__IO:1} 2 (v5 v4 v0 v1 v2 v3))
   (MKCLOSURE v7 Prelude.{<*>_Applicative__IO:2} 2 (v4 v0 v1 v2 v3))
   (APPLY RVAL v6 v7)))

(defun Prelude.{<*>_Applicative__IO:2}
  (v0 v1 v2 v3 v4 v6 v5)
  ((DECLARE v7)
   (START)
   (APPLY v7 v0 v6)
   (CALL RVAL True PrimIO.io_pure (DISCARD v7 v5))))

(defun Prelude.{<*>_Applicative__IO:1}
  (v0 v1 v2 v3 v4 v5 v7 v6)
  ((DECLARE v8)
   (START)
   (APPLY v8 v0 v6)
   (CALL
    RVAL
    True
    "PrimIO.case block in 181(302)"
    (DISCARD DISCARD DISCARD v7 DISCARD v8))))

(defun Prelude.{<*>_Applicative__IO:0}
  (v0 v1 v2 v3 v4 v6 v5)
  ((DECLARE v7)
   (START)
   (APPLY v7 v0 v5)
   (CALL
    RVAL
    True
    "PrimIO.case block in 181(302)"
    (DISCARD DISCARD DISCARD v6 DISCARD v7))))

(defun Prelude./=_Eq__Integer
  (v0 v1)
  ((DECLARE v2)
   (START)
   (CALL v2 False Prelude.==_Eq__Integer (v0 v1))
   (CALL RVAL True Prelude.not (v2))))

(defun Prelude./=_Eq__Int
  (v0 v1)
  ((DECLARE v2)
   (START)
   (CALL v2 False Prelude.==_Eq__Int (v0 v1))
   (CALL RVAL True Prelude.not (v2))))

(defun Prelude.+_Num__Integer (v0 v1) ((START) (OP RVAL +Integer v0 v1)))

(defun Prelude.+_Num__Int (v0 v1) ((START) (OP RVAL +Int v0 v1)))

(defun Prelude.*_Num__Integer (v0 v1) ((START) (OP RVAL *Integer v0 v1)))

(defun Prelude.*_Num__Int (v0 v1) ((START) (OP RVAL *Int v0 v1)))

(defun Prelude.{_:6923}
  (v0 v1 v2 v3)
  ((DECLARE v4)
   (START)
   (MKCLOSURE v4 Prelude.{{_:6923}:0} 1 (v0 v1 v2 v3))
   (MKCON RVAL 1 (v1 v4))))

(defun Prelude.{{_:6923}:0} (v0 v1 v2 v3 v4) ((START) (ASSIGN RVAL v0)))

(defun Prelude.{_:6897} (v0 v1 v2) ((START) (MKCON RVAL 101 (v1))))

(defun Prelude.{_:6896}
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (START)
   (MKCLOSURE v2 Prelude.{{_:6896}:0} 1 (v0 v1))
   (MKCON v3 1 (v1 v2))
   (MKCON RVAL 101 (v3))))

(defun Prelude.{{_:6896}:0} (v0 v1 v2) ((START) (ASSIGN RVAL v0)))

(defun Prelude.{_:6854}
  (v0 v1 v2)
  ((START) (CALL RVAL True Prelude.{f:6850} (v1))))

(defun Prelude.{_:6853}
  (v0 v1)
  ((DECLARE v2)
   (START)
   (MKCLOSURE v2 Prelude.{{_:6853}:0} 1 (v0 v1))
   (MKCON RVAL 1 (v1 v2))))

(defun Prelude.{{_:6853}:0} (v0 v1 v2) ((START) (ASSIGN RVAL v0)))

(defun Prelude.{f:6850} (v0) ((START) (MKCON RVAL 101 (v0))))

(defun Prelude.pure
  (v0 v1 v2)
  ((DECLARE v3)
   (DECLARE v4)
   (DECLARE v5)
   (START)
   (CASE v2
     (nodefault)
     (0 (
       (v3 v2 0)
       (v4 v2 1)
       (v5 v2 2)
       (MKCLOSURE RVAL Prelude.{pure:0} 1 (v3 v4 v5 v0 v1 v2)))))))

(defun Prelude.{pure:0}
  (v0 v1 v2 v3 v4 v5 v6)
  ((DECLARE v7)
   (START)
   (APPLY v7   v1 DISCARD)
   (APPLY RVAL v7 v6)))

(defun Prelude.not
  (v0)
  ((START)
   (CASE v0
     (nodefault)
     (0 ((MKCON RVAL 1 ())))
     (1 ((MKCON RVAL 0 ()))))))

(defun Prelude.natToInteger
  (v0)
  ((DECLARE v2)
   (DECLARE v1)
   (DECLARE v3)
   (DECLARE v4)
   (START)
   (CONSTCASE v0
     (default (
       (MKCONSTANT v2 1)
       (OP v1 -Integer v0 v2)
       (MKCONSTANT v3 1)
       (CALL v4 False Prelude.fromInteger_Num__Integer (v3))
       (CALL RVAL True Prelude.+_Num__Integer (v4 v1))))
     (0 ((MKCONSTANT RVAL 0))))))

(defun Prelude.integerToNat
  (v0)
  ((DECLARE v1)
   (DECLARE v2)
   (DECLARE v3)
   (START)
   (MKCONSTANT v1 0)
   (OP v2 <=Integer v0 v1)
   (CONSTCASE v2
     (default ((MKCON v3 0 ())))
     (0       ((MKCON v3 1 ()))))
   (CALL RVAL True "Prelude.case block in 3412(3660)" (v0 v3))))

(defun Prelude.intToBool
  (v0)
  ((START)
   (CONSTCASE v0
     (default ((MKCON RVAL 0 ())))
     (0       ((MKCON RVAL 1 ()))))))

(defun Prelude.id (v0 v1) ((START) (ASSIGN RVAL v1)))

(defun Prelude.fromInteger
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (START)
   (CASE v1
     (nodefault)
     (0 (
       (v2 v1 0)
       (v3 v1 1)
       (v4 v1 2)
       (MKCLOSURE RVAL Prelude.{fromInteger:0} 1 (v2 v3 v4 v0 v1)))))))

(defun Prelude.{fromInteger:0}
  (v0 v1 v2 v3 v4 v5)
  ((START) (APPLY RVAL v2 v5)))

(defun Prelude.div
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (START)
   (CASE v1
     (nodefault)
     (0 (
       (v2 v1 0)
       (v3 v1 1)
       (v4 v1 2)
       (MKCLOSURE RVAL Prelude.{div:0} 2 (v2 v3 v4 v0 v1)))))))

(defun Prelude.{div:0}
  (v0 v1 v2 v3 v4 v6 v5)
  ((DECLARE v7)
   (START)
   (APPLY v7   v1 v6)
   (APPLY RVAL v7 v5)))

(defun Prelude.>
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (DECLARE v5)
   (DECLARE v6)
   (DECLARE v7)
   (DECLARE v8)
   (DECLARE v9)
   (START)
   (CASE v1
     (nodefault)
     (0 (
       (v2 v1 0)
       (v3 v1 1)
       (v4 v1 2)
       (v5 v1 3)
       (v6 v1 4)
       (v7 v1 5)
       (v8 v1 6)
       (v9 v1 7)
       (MKCLOSURE RVAL Prelude.{>:0} 2 (v2 v3 v4 v5 v6 v7 v8 v9 v0 v1)))))))

(defun Prelude.{>:0}
  (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v11 v10)
  ((DECLARE v12)
   (START)
   (APPLY v12  v3  v11)
   (APPLY RVAL v12 v10)))

(defun Prelude.==
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (START)
   (CASE v1
     (nodefault)
     (0 (
       (v2 v1 0)
       (v3 v1 1)
       (MKCLOSURE RVAL Prelude.{==:0} 2 (v2 v3 v0 v1)))))))

(defun Prelude.{==:0}
  (v0 v1 v2 v3 v5 v4)
  ((DECLARE v6)
   (START)
   (APPLY v6   v0 v5)
   (APPLY RVAL v6 v4)))

(defun Prelude.<
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (DECLARE v5)
   (DECLARE v6)
   (DECLARE v7)
   (DECLARE v8)
   (DECLARE v9)
   (START)
   (CASE v1
     (nodefault)
     (0 (
       (v2 v1 0)
       (v3 v1 1)
       (v4 v1 2)
       (v5 v1 3)
       (v6 v1 4)
       (v7 v1 5)
       (v8 v1 6)
       (v9 v1 7)
       (MKCLOSURE RVAL Prelude.{<:0} 2 (v2 v3 v4 v5 v6 v7 v8 v9 v0 v1)))))))

(defun Prelude.{<:0}
  (v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v11 v10)
  ((DECLARE v12)
   (START)
   (APPLY v12  v2  v11)
   (APPLY RVAL v12 v10)))

(defun Prelude.+
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (START)
   (CASE v1
     (nodefault)
     (0 (
       (v2 v1 0)
       (v3 v1 1)
       (v4 v1 2)
       (MKCLOSURE RVAL Prelude.{+:0} 2 (v2 v3 v4 v0 v1)))))))

(defun Prelude.{+:0}
  (v0 v1 v2 v3 v4 v6 v5)
  ((DECLARE v7)
   (START)
   (APPLY v7   v0 v6)
   (APPLY RVAL v7 v5)))

(defun Prelude.*
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (DECLARE v4)
   (START)
   (CASE v1
     (nodefault)
     (0 (
       (v2 v1 0)
       (v3 v1 1)
       (v4 v1 2)
       (MKCLOSURE RVAL Prelude.{*:0} 2 (v2 v3 v4 v0 v1)))))))

(defun Prelude.{*:0}
  (v0 v1 v2 v3 v4 v6 v5)
  ((DECLARE v7)
   (START)
   (APPLY v7   v1 v6)
   (APPLY RVAL v7 v5)))

(defun "PrimIO.case block in 323(410)"
  (v0 v1 v2 v3)
  ((DECLARE v4)
   (START)
   (ASSIGN v4 v3)
   (CALL RVAL True PrimIO.unsafeDestroyWorld (DISCARD DISCARD v4))))

(defun "PrimIO.case block in 196(324)"
  (v0 v1 v2 v3 v4 v5 v6 v7)
  ((DECLARE v8) (START) (ASSIGN v8 v7) (APPLY RVAL v8 v6)))

(defun "PrimIO.case block in 181(302)"
  (v0 v1 v2 v3 v4 v5)
  ((DECLARE v6)
   (DECLARE v7)
   (START)
   (ASSIGN v6 v5)
   (APPLY v7 v3 v6)
   (CALL
    RVAL
    True
    "PrimIO.case block in 196(324)"
    (DISCARD DISCARD DISCARD DISCARD DISCARD v6 DISCARD v7))))

(defun PrimIO.{_:294}
  (v0 v1 v2)
  ((DECLARE v3)
   (START)
   (MKCLOSURE v3 PrimIO.{{_:294}:0} 1 (v0 v1 v2))
   (MKCON RVAL 1 (v1 v3))))

(defun PrimIO.{{_:294}:0} (v0 v1 v2 v3) ((START) (MKCON RVAL 101 (v0))))

(defun PrimIO.unsafePerformIO
  (v0 v1)
  ((DECLARE v2)
   (DECLARE v3)
   (START)
   (ASSIGN v2 v1)
   (MKCLOSURE v3 PrimIO.{unsafePerformIO:0} 1 (v2 v0 v1))
   (CALL RVAL True PrimIO.unsafeCreateWorld (DISCARD v3))))

(defun PrimIO.{unsafePerformIO:0}
  (v0 v1 v2 v3)
  ((DECLARE v4)
   (START)
   (APPLY v4 v0 v3)
   (CALL RVAL True "PrimIO.case block in 323(410)" (DISCARD v0 DISCARD v4))))

(defun PrimIO.unsafeDestroyWorld (v0 v1 v2) ((START) (ASSIGN RVAL v2)))

(defun PrimIO.unsafeCreateWorld
  (v0 v1)
  ((DECLARE v2) (START) (MKCONSTANT v2 %MkWorld) (APPLY RVAL v1 v2)))

(defun PrimIO.io_pure (v0 v1 v2) ((START) (ASSIGN RVAL v1)))

(defun PrimIO.io_bind
  (v0 v1 v2)
  ((DECLARE v3)
   (START)
   (ASSIGN v3 v2)
   (MKCLOSURE RVAL PrimIO.{io_bind:0} 2 (v3 v0 v1 v2))))

(defun PrimIO.{io_bind:0}
  (v0 v1 v2 v3 v5 v4)
  ((DECLARE v6)
   (START)
   (APPLY v6 v0 v4)
   (CALL
    RVAL
    True
    "PrimIO.case block in 181(302)"
    (DISCARD DISCARD DISCARD v5 DISCARD v6))))

(defun Builtin.assert_total (v0 v1) ((START) (ASSIGN RVAL v1)))
