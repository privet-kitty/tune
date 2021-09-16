(defsystem "tune"
  :version "0.1.0"
  :author "Hugo Sansaqua"
  :license "MIT"
  :depends-on ("jsown")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Define tuned parameters"
  :in-order-to ((test-op (test-op "tune/tests"))))

(defsystem "tune/tests"
  :author ""
  :license ""
  :depends-on ("tune"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for tune"
  :perform (test-op (op c) (symbol-call :rove :run c)))
