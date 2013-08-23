(defun pad-ocaml-project-pfff ()
  (interactive)

  (setq 
   pad-ocaml-project-path "/home/pad/github/zamcov"
   pad-ocaml-project-subdirs 
   (split-string 
    "clibs mllibs"
    )
   pad-ocaml-project-toplevel "zamcov.top"
   )

  ; --------------------------------------------------------------------------
  ; zamcov
  ; --------------------------------------------------------------------------
  (setq
   pad-ocaml-project-prog     "zamcov-run.byte"
   pad-ocaml-project-args 
   (join-string 
    (list 
     " "
     (case 0
       (0 "/home/pad/github/zamcov/tests/foo")
       )
     )
    )
   )
)



