(asdf:defsystem para
  :depends-on (:serapeum :metabang-bind :trivia :alexandria :scaena :fare-memoization :lparallel)
  :components ((:file "objet-petit-a")
               (:file "para" :depends-on ("objet-petit-a"))
               (:file "benchmark" :depends-on ("para"))
               (:file "smyth" :depends-on ("benchmark"))
               (:file "min-max-heap")
               (:file "bfs" :depends-on ("para" "min-max-heap"))
               (:file "l2" :depends-on ("benchmark"))))
