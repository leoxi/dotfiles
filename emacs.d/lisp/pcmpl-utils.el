;;; pcmpl-utils --- pcomplete a lot

;; Author: Wei Zhao <kaihaosw@gmail.com>
;; Created: 2015-07-19
;; Updated: 2015-07-19


(provide 'pcmpl-utils)

;; du
(defvar du-options '("-H" "-L" "-P" "-a"
                     "-s" "-d" "-c" "-h"
                     "-k" "-m" "-g" "-x"
                     "-I"))

(defun pcomplete/du ()
  (while
      (if (pcomplete-match "^-" 0)
          (pcomplete-here du-options)
        (pcomplete-here (pcomplete-entries)))))


;; df
(defvar df-options '("-b" "-H" "-h" "-k" "-m"
                     "-g" "-P" "-ailn" "-T" "-t"))

(defun pcomplete/df ()
  (while
      (if (pcomplete-match "^-" 0)
          (pcomplete-here df-options)
        (pcomplete-here (pcomplete-entries)))))


;; activator
(defvar activator-commands '("ui" "new" "list-templates"))

(defvar activator-tasks-commands '("clean" "compile" "console"
                                   "consoleProject" "consoleQuick" "copyResources"
                                   "doc" "mochaOnly" "package"
                                   "packageBin" "packageDoc" "packageSrc"
                                   "publish" "publishLocal" "publishM2"
                                   "run" "runMain" "test"
                                   "testOnly" "testQuick" "update"))

(defvar activator-all-commands (append activator-commands
                                       activator-tasks-commands))

(defvar activator-options '("-h" "-help"
                            "-v" "-verbose"
                            "-d" "-debug"
                            "-mem"
                            "-jvm-debug"
                            "-java-home"
                            "-Dkey=val"
                            "-J-X"))

(defun pcomplete/activator ()
  (when (pcomplete-match "^-" 0)
    (pcomplete-here activator-options))
  (while
      (pcomplete-here* activator-all-commands)))

;;; pcmpl-utils ends here
