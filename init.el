;;; init.el --- Brady's emacs init -*- lexical-binding: t -*-
;;; Commentary:

;; This file contains the code required to tangle and load my literate
;; config from README.org

;;; Code:

(defun brady/tangle-load-config (&optional arg)
  """Tangle and load the config from README.org. By default, this
  function will only re-tangle the file if the current org file
  is newer than the existing elisp file. If given a prefix
  argument, it will always re-tangle."""
  (interactive "P")
  (let ((org-file (expand-file-name "README.org" user-emacs-directory))
	(el-file (expand-file-name "config.el" user-emacs-directory)))
    (when (or (file-newer-than-file-p org-file el-file) arg)
      (org-babel-tangle-file org-file el-file))
    (load-file el-file)))

(require 'org)
(brady/tangle-load-config)
(provide 'init)
;;; init.el ends here
