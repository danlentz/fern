(require :sb-bench)

(sb-bench:defbenchmark generic-+ ((x y z) (0 0 0) :seconds 1.0)
  (let ((r 42))
    (sb-bench:open-code (100)
      (setf r (+ (+ x x)
                 (+ x y)
                 (+ x z)
                 (+ y y)
                 (+ y z)
                 (+ z z))))
    r))

(print (sb-bench:run-benchmarks))
