;; Korean
(require 'cl)
(when enable-multibyte-characters
  (set-language-environment "Korean"))

(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))
(require 'hangul)

;; M-x remap
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(add-to-list 'yas/extra-mode-hooks 'ruby-mode-hook)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets/text-mode/")

;; ECB
(load-file "~/.emacs.d/cedet-1.0pre4/common/cedet.el")
(add-to-list 'load-path "~/.emacs.d/ecb-2.32/")
(require 'ecb)

(setq ecb-tip-of-the-day nil)
(setq ecb-layout-name "left11")
(setq echo-windows-with 0.25)
(custom-set-variables
 '(ecb-fix-window-size (quote width))
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.32"))


;; ido
;; (load_file "~/.emacs.d/ido.el")
;; minibuff of vim
(require 'ido)
(ido-mode t)
(ido-mode 'buffer)
(global-set-key [(ctrl tab)] 'ido-switch-buffer)
(global-set-key [(ctrl shift tab)] 'ido-switch-buffer)
(add-hook
  'ido-setup-hook
    (lambda ()
      (define-key ido-buffer-completion-map [(ctrl tab)] 'ido-next-match)
      (define-key ido-buffer-completion-map [(ctrl shift tab)] 'ido-prev-match)))

(require 'htmlize)

;; command line completion
(load-file "~/.emacs.d/shell-command.el")
(require 'shell-command)
(shell-command-completion-mode)

;; howm
;(load-file "/opt/local/share/emacs/site-lisp/howm/howm.el")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/howm")
(require 'howm)

;(setq planner-project "WikiPlanner")
;(setq muse-project-alist
;      '(("WikiPlanner"
;         ("~/plans"   ;; Or wherever you want your planner files to be
;          :default "index"
;          :major-mode planner-mode
;          :visit-link planner-visit-link))))
;(require 'planner)

;; buffer
(one-buffer-one-frame-mode nil)

;; better buffer switching
(iswitchb-mode t)

;; compress
(auto-compression-mode t)

;; gud 설정
(setq gdb-many-windows t)
(global-set-key [f5]  'gud-run)
(global-set-key [f9]  'gud-break)
(global-set-key [f10] 'gud-next)
(global-set-key [f11] 'gud-step)
(global-set-key [(shift f11)] 'gud-finish)
(global-set-key [(shift f10)] '(lambda ()
                                (interactive)
                                (call-interactively 'gud-tbreak)
                                (call-interactively 'gud-cont)))
(global-set-key (kbd "C-x C-a C-c") 'gud-cont)
(global-set-key (kbd "C-x C-a C-w") 'gud-watch)

(defun gdb-mode-additional-keys()
  "Key bindings to add to `gdb-mode'."
  (gud-def gud-kill "kill" "\C-k" "Kill the program.")
  (gud-def gud-quit "quit" "\C-q" "Quit the program.")
)

(add-hook 'gdb-mode-hook 'gdb-mode-additional-keys)

;; reload
(global-auto-revert-mode 1)

;; backup
(setq make-backup-files nil)

;; svn
(load-file "~/.emacs.d/psvn.el")

;; dic
(require 'thingatpt)
(setq yoonkn/dic-dicurl "http://kr.engdic.yahoo.com/search/engdic?p=%s")
(defun yoonkn/dic-at-point ()
  (interactive)
  (browse-url (format yoonkn/dic-dicurl (word-at-point))))
(global-set-key "\C-c dic" 'yoonkn/dic-at-point)

;; column no
(column-number-mode t)
(autoload 'align-cols "align" "Align text in the region." t)

;; line no.
(line-number-mode 1)

(require 'linum)
(linum-mode 1)

;; tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; search
(setq-default case-fold-search nil) ; require exact match
(setq-default case-relace nil)      ; never change case when replaceing

;; toolbar mode
(tool-bar-mode -1)

;; mouse
(setq mouse-yank-at-point t)

;; unicode-shell
(defun unicode-shell()
  "Execute the shell buffer in UTF-8 encoding.
  Note that you'll need to set the environment variable LANG and others
  appropriately."
  (interactive)
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        (coding-system-require-warning t))
    (call-interactively 'shell)))

(global-set-key "\C-cd" 'unicode-shell)

;; rails
(setq load-path (cons "~/.emacs.d/rails" load-path))
(require 'rails)

;; C programming mode
(defun c-mode ()
  "C++ mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "bsd")
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2))

(global-set-key "\C-cc" 'compile)

;; c++
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; c++ indentation
(defconst my-c-style
  '((c-tab-always-indent          . t)
    (c-comment-only-line-offset   . 2)
    (c-hanging-braces-alist       . ((substatement-open after)
                                     (brace-list-open)))
    (c-hanging-colons-alist       . ((member-init-intro before)
                                     (inher-intro)
                                     (case-label after)
                                     (label after)
                                     (access-label after)))
    (c-cleanup-list               . (scope-operator
                                     empty-defun-braces
                                     defun-close-semi))
    (c-offsets-alist              . ((arglist-close . c-lineup-arglist)
                                     (substatement-open . 0)
                                     (case-label        . 0)
                                     (block-open        . 0)
                                     (namespace-open . 0)
                                     (namespace-close . 0)
                                     (innamespace . 0)
                                     (inextern-lang . 0)
                                     (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    )
  "My C++ Programming Style")


;; c,c++mode,java Makefile
;;
(add-hook
 'c-mode-common-hook
 (lambda ()
   (unless (file-exists-p "Makefile")
     (set (make-local-variable 'compile-command)
          (let ((file (file-name-nondirectory buffer-file-name)))
            (format "%s -o %s %s %s %s"
                    (or (getenv "CC") "gcc")
                    (file-name-sans-extension file)
                    (or (getenv "CPPFLAGS") "")
                    (or (getenv "CFLAGS") "-Wall -g")
                    file))))))

(add-hook
 'c++-mode-hook
 (lambda ()
   (unless (file-exists-p "Makefile")
     (set (make-local-variable 'compile-command)
          (let ((file (file-name-nondirectory buffer-file-name)))
            (format "%s -o %s %s %s %s"
                    (or (getenv "CC") "g++")
                    (file-name-sans-extension file)
                    (or (getenv "CPPFLAGS") "")
                    (or (getenv "CFLAGS") "-Wall -g")
                    file))))))

(add-hook
 'java-mode-hook
 (lambda ()
   (unless (file-exists-p "Makefile")
     (set (make-local-variable 'compile-command)
          (let ((file (file-name-nondirectory buffer-file-name)))
            (format "%s %s %s"
                    (or (getenv "JAVAC") "javac")
                    (or (getenv "JAVACFLAGS") "")
                    file))))))

;; customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  (c-add-style "PERSONAL" my-c-style t)
  (setq tab-width 2
        ;;indent-tabs-mode t
        c-basic-offset 2)
  (c-toggle-auto-state -1)              ; disable auto-newline mode
  (c-toggle-hungry-state 1)             ; enable hungry-delete mode

  ;; return 으로 indent 까지 같이 하도록했다
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)

  ;; C-c RET 로 .h .cpp 간의 전환을 하도록 했다.
  (define-key c-mode-base-map [(control c)(return)] 'ff-find-other-file)

  ;; ecb 가 켜진 상태라면, C-c C-c 를 ecb-goto-window-methods 에 바인딩했다.
  (when (fboundp 'ecb-goto-window-methods)
    (define-key c-mode-base-map [(control c)(control c)] 'ecb-goto-window-methods))

  ;; cedet 가 로딩된상태라면 C-Ret 를 semantic-analyze-possible-completions 로 매핑했다.
  ;; semantic-ia-complete-symbol 등 여러선택이 있지만 이놈이 맘에들었다.
  ;; ...다가 다시 semantic-ia-complete-symbol 로 바꿨다. 새로 버퍼가 열리면서 인자까지 보여주는게 맘에 들었었는데,
  ;; 버퍼가 다시 사라지지 않은게 이유
  (when (fboundp 'semantic-analyze-possible-completions)
    (define-key c-mode-base-map [(meta return)] 'semantic-ia-complete-symbol)
    (define-key c-mode-base-map [(control return)] 'semantic-complete-analyze-inline))


  (c-set-offset 'substatement-open 0)
  (c-set-offset 'member-init-intro '++)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'label 0)
  (c-set-offset 'arglist-intro '+)

  (hs-minor-mode 1)
  (hide-ifdef-mode 1)
  (setq hide-ifdef-lines t)             ; #if 등까지 가려버리자
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; TODO, BUG 등에 강조표시
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(FIXME\\):" 1 c-nonbreakable-space-face prepend)
                          ("\\<\\(TODO\\):" 1 c-nonbreakable-space-face prepend)
                          ("\\<\\(BUG\\):" 1 c-nonbreakable-space-face prepend)
                          ("\\<\\(NOTE\\):" 1 c-nonbreakable-space-face prepend)))
;; backup 설정
;; 나도 작업 디렉토리에 백업파일이 남는것을 싫어한다
;; backup-by-coping 이라는 중요한 변수가 있는데, 메뉴얼을 봐라
(setq backup-directory-alist '(("." . "~/.emacs_backup")))
(setq
 version-control t                      ; 백업을 여러벌한다
 kept-old-versions 2                    ; oldest 백업 2개는 유지
 kept-new-versions 3                    ; newest 백업 3개 유지
 delete-old-versions t)                 ; 지울때마다 묻지 않도록

;; ediff 설정

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; vim 의 % 처럼, 짝이 맞는 괄호를 찾아주는 놈
;; http://www.emacswiki.org/cgi-bin/wiki/MatchParenthesis
;; 에서 가져왔다고 한다.
(defun match-paren ()
  "% command of vi"
  (interactive)
  (let ((char (char-after (point))))
    (cond ((memq char '(?\( ?\{ ?\[))
           (forward-sexp 1)
           (backward-char 1))
          ((memq char '(?\) ?\} ?\]))
           (forward-char 1)
           (backward-sexp 1))
          (t (call-interactively 'self-insert-command)))))

;; % 에 바로 바인딩했다.
;; 필요하다면, C-q % 를 쓰자
(global-set-key (kbd "%") 'match-paren)

;; use xcscope.el
;; 요거 존내 좋구나
;; gtags 의 깔끔함이 더 맘에 들지만 이것도 세팅해둔다
;; cscope-database-regexps 설정은 office.el 에 해둔다.
;; cscope 설치후엔 INCLUDEDIRS 환경변수를 잡는것을 잊지 말자
;; (require 'xcscope)

;;(defun yoonkn%select-other-window ()
;;  "cscope-bury-buffer 가 오동작하길래 땜빵으로 만들었다"
;;  (interactive)
;;  (message "cscope-bury-buffer 가 정상작동하는지 확인해보고 이거 지워라!")
;;  (select-window (previous-window))
;;  (delete-other-windows))

;;(define-key cscope-list-entry-keymap [(return)] 'cscope-select-entry-one-window)
;;(define-key cscope-list-entry-keymap "q" 'yoonkn%select-other-window)

;;;; quack.el
;; SICP 를 공부하기 위해서 scheme 모드를 설치했다.
;; http://www.neilvandyke.org/quack/
;; (setq quack-pltcollect-dirs '("C:/Program Files/PLT/collects/"))
;; (require 'quack)

;;;; RFC 가져오기
;; http://emacswiki.org/cgi-bin/wiki/JorgenSchaefersEmacsConfig
;; M-x rfc 후 0 또는 원하는 rfc 문서번호를 넣는다.
;; RFC 를 가져오는것 자체보다, 여러가지로 응용해볼만한 함수
(defun rfc (num)
  "Show RFC NUM in a buffer."
  (interactive "nRFC (0 for index): ")
  (let ((url (if (zerop num)
                 "http://www.ietf.org/iesg/1rfc_index.txt"
               (format "http://www.ietf.org/rfc/rfc%i.txt" num)))
        (buf (get-buffer-create "*RFC*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (let ((proc (start-process "wget" buf "wget" "-q" "-O" "-" url)))
          (set-process-sentinel proc 'rfc-sentinel))
        (message "Getting RFC %i..." num)))))

(defun rfc-sentinel (proc event)
  "Sentinel for `rfc'."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (view-mode 1)
    (when (fboundp 'rfcview-mode)
      (rfcview-mode)))
  (display-buffer (process-buffer proc)))


;;;; tramp-mode
;; /plink:yoonkn@111.111.111.11:/home/yoonkn/src/ 형태로 열면 된다.
;; 아직 자주 쓰지 않으니 상세한 설정은 나중에
(require 'tramp)
(setq tramp-auto-save-directory "/tmp"
      tramp-default-method "plink"
      tramp-password-end-of-line "\r\n")

;;;; ebowse

;; ebrowse-build.bat
;;
;; @echo off
;; set find=c:/bin/find.exe
;; set tr=c:/bin/tr.exe
;; set ebrowse=c:/"Program Files"/Emacs/emacs/bin/ebrowse.exe

;; set srcdir=%1
;; set list1=%srcdir%/.ebrowse.filelist.1
;; set list2=%srcdir%/.ebrowse.filelist.2
;; set brdat=%srcdir%/BROWSE

;; %find% %srcdir% -type f -regex ".*\.\(h\|\hh\|\H\|hpp\|hxx\)" >  %list1%
;; %find% %srcdir% -type f -regex ".*\.\(c\|\cc\|\C\|cpp\|cxx\)" >> %list1%
;; %tr% \\ / < %list1% > %list2%
;; %ebrowse% -f %list2% -o %brdat%
;; rm -f %list1%
;; rm -f %list2%

;; @echo on
;;
(defun my-ebrowse-build (srcdir)
  (w32-shell-execute "OPEN"
                     "ebrowse-build.bat"
                     srcdir))

(defun my-ebrowse-switch ()
  (interactive)
  (let* ((srcdir (expand-file-name (or (and (boundp 'yoonkn-build-option)
                                            (getf yoonkn-build-option :src))
                                       default-directory)))
         (ebrowse-file (format "%s/BROWSE" srcdir))
         (ebrowse-buffer (get-buffer "*Tree*")))
    (if current-prefix-arg
        (progn
          (when ebrowse-buffer (kill-buffer ebrowse-buffer))
          (my-ebrowse-build srcdir))
      (cond (ebrowse-buffer
             (switch-to-buffer ebrowse-buffer))
            ((file-exists-p ebrowse-file)
             (find-file ebrowse-file))
            (t
             (my-ebrowse-build srcdir))))))

(global-set-key [(meta f12)] 'my-ebrowse-switch)

;; *, # of vim
;;
(global-set-key [C-M-down] 'vjo-forward-current-word-keep-offset)
(global-set-key [C-M-up] 'vjo-backward-current-word-keep-offset)

(defun vjo-forward-current-word-keep-offset ()
  " (Vagn Johansen 1999)"
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" (thing-at-point 'symbol) "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point))) ; offset from start of symbol/word
    (setq offset (- (length curword) offset)) ; offset from end
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-forward re-curword nil t)
        (backward-char offset)
      ;; else
      (progn (goto-char (point-min))
             (if (re-search-forward re-curword nil t)
                 (progn (message "Searching from top. %s" (what-line))
                        (backward-char offset))
               ;; else
               (message "Searching from top: Not found"))
             ))
    (setq case-fold-search old-case-fold-search)
    ))
(defun vjo-backward-current-word-keep-offset ()
  " (Vagn Johansen 2002)"
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" curword "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point))) ; offset from start of symbol/word
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-backward re-curword nil t)
        (forward-char offset)
      ;; else
      (progn (goto-char (point-max))
             (if (re-search-backward re-curword nil t)
                 (progn (message "Searching from bottom. %s" (what-line))
                        (forward-char offset))
               ;; else
               (message "Searching from bottom: Not found"))
             ))
    (setq case-fold-search old-case-fold-search)
    ))

;; transparency
(set-frame-parameter nil 'alpha 100)  ; set initial value
(defun transparency-set-value (numb)
  "Set level of transparency for current Aquamacs frame"
  (interactive "nEnter transparency level in range 0-100: ")
  (if (> numb 100)
      (message "Error! The maximum value for transparency is 100!")
    (if (< numb 0)
  (message "Error! The minimum value for transparency is 0!")
      (set-frame-parameter nil 'alpha numb)
      )
    )
  )
(defun transparency-increase ()
  "Increase level of transparency for Aquamacs frame"
  (interactive)
  (if (> (frame-parameter nil 'alpha) 0)
      (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) -5))
    )
  )
(defun transparency-decrease ()
  "Decrease level of transparency for Aquamacs frame"
  (interactive)
  (if (< (frame-parameter nil 'alpha) 100)
      (set-frame-parameter nil 'alpha (+ (frame-parameter nil 'alpha) +5))
    )
  )

;; global c warm mode
;; (global-cwarn-mode 1)
