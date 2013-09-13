(defun pad-ocaml-project-pfff ()
  (interactive)

  (setq 
   pad-ocaml-project-path "/home/pad/github/fork-zamcov"
   pad-ocaml-project-subdirs 
   (split-string 
    "commons
     interpreter
     clibs mllibs"
    )
   pad-ocaml-project-toplevel "zamcov.top"
   )

  ; --------------------------------------------------------------------------
  ; zamcov
  ; --------------------------------------------------------------------------
  (setq
   pad-ocaml-project-prog     "zamcov_test"
   pad-ocaml-project-args 
   (join-string 
    (list 
     " "
     (case 0
       (0 "/home/pad/github/fork-zamcov/tests/foo")
       (1 "/home/pad/pfff/pfff")
       )
     )
    )
   )
)



