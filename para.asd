(asdf:defsystem para
  :depends-on (:serapeum :metabang-bind :trivia :alexandria :fare-memoization :lparallel :fset
                         :trivial-arguments :trivial-package-local-nicknames)
  :components ((:file "scaena")
               (:file "objet-petit-a")
               (:file "para" :depends-on ("objet-petit-a"))
               (:file "benchmark" :depends-on ("para"))
               (:file "smyth" :depends-on ("benchmark"))
               (:file "min-max-heap")
               (:file "bfs" :depends-on ("para" "min-max-heap"))
               (:file "l2" :depends-on ("benchmark"))))
