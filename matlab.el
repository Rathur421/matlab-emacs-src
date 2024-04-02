;;; matlab.el --- major mode for MATLAB(R) dot-m files

;; Author: Matt Wette <mwette@alumni.caltech.edu>,
;;         Eric M. Ludlam <eludlam@mathworks.com>
;; Maintainer: Eric M. Ludlam <eludlam@mathworks.com>
;; Created: 04 Jan 91
;; Keywords: MATLAB(R)
;; Version:

(defconst matlab-mode-version "5.0"
  "Current version of MATLAB(R) mode.")

;;
;; Copyright (C) 1997-2022 Eric M. Ludlam
;; Copyright (C) 1991-1997 Matthew R. Wette
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; This major mode for GNU Emacs provides support for editing MATLAB(R) dot-m
;; files.  It automatically indents for block structures (including nested
;; functions), line continuations (e.g., ...), and comments.
;;
;; Additional features include auto-fill including auto-additions of
;; ellipsis for commands, and even strings.  Block/end construct
;; highlighting as you edit.  Primitive code-verification and
;; identification.  Templates and other code editing functions.
;; Advanced symbol completion.  Code highlighting via font-lock.
;; There are many navigation commands that let you move across blocks
;; of code at different levels.
;;
;; Lastly, there is support for running MATLAB(R) in an Emacs buffer,
;; with full shell history and debugger support (when used with the db
;; commands.)  The shell can be used as an online help while editing
;; code, providing help on functions, variables, or running arbitrary
;; blocks of code from the buffer you are editing.

;;; Code:

(require 'matlab-compat)
(require 'matlab-syntax)
(require 'easymenu)
(require 'derived)

;; (eval-when-compile
;;   (require 'elec-pair))


;;; User-changeable variables =================================================
;;

;; Variables which the user can change
(defgroup matlab nil
  "MATLAB(R) mode."
  :prefix "matlab-"
  :group 'languages)

(defcustom matlab-mode-for-new-mfiles 'maybe
  "*Enter `matlab-mode' for new *.m files.
The `matlab' package will automatically enter `matlab-mode' when
the first part of a *.m file is doesn't contain Objective-C
comments or '#' characters. If you want new (empty) files to
automatically enter `matlab-mode', specify this item as
t (always). If you specify 'maybe, new files will enter
`matlab-mode' when you have an existing MATLAB buffer. Specifying
nil (never) means that new *.m files will not enter
`matlab-mode', and with default Emacs settings they will enter
`objc-mode'"
  :group 'matlab
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "Maybe" maybe)))

(defcustom matlab-indent-level 4
  "*The basic indentation amount in `matlab-mode'."
  :group 'matlab
  :type 'integer)

(defcustom matlab-continuation-indent-level 4
  "*Basic indentation after continuation if no other methods are found."
  :group 'matlab
  :type 'integer)

(defcustom matlab-array-continuation-indent-level 2
  "*Basic indentation after continuation within an array if no other methods are found."
  :group 'matlab
  :type 'integer)

(defcustom matlab-cont-requires-ellipsis t
  "*Specify if ellipses are required at the end of a line for continuation.
Future versions of Matlab may not require ellipses ... , so a heuristic
determining if there is to be continuation is used instead."
  :group 'matlab
  :type 'integer)

(defcustom matlab-case-indent-level '(2 . 2)
  "*How far to indent case/otherwise statements in a switch.
This can be an integer, which is the distance to indent the CASE and
OTHERWISE commands, and how far to indent commands appearing in CASE
and OTHERWISE blocks.  It can also be a cons cell which is of form
  (CASEINDENT . COMMANDINDENT)
where CASEINDENT is the indentation of the CASE and OTHERWISE
statements, and COMMANDINDENT is the indentation of commands appearing
after the CASE or OTHERWISE command.

Note: Currently a bug exists if:
  CASEINDENT+COMMANDINDENT != `matlab-indent-level'
so if you customize these variables, follow the above rule, and you
should be ok."
  :group 'matlab
  :type 'sexp)

(defcustom matlab-indent-past-arg1-functions
  "\\_<\\([sg]et\\(_param\\)?\\|waitfor\\|notify\\)\\_>"
  "*Regex describing functions whose first arg is special.
This specialness means that all following parameters which appear on
continued lines should appear indented to line up with the second
argument, not the first argument."
  :group 'matlab
  :type 'string)

(defcustom matlab-arg1-max-indent-length 15
  "*The maximum length to indent when indenting past arg1.
If arg1 is exceptionally long, then only this number of characters
will be indented beyond the open paren starting the parameter list."
  :group 'matlab
  :type 'integer)

(defcustom matlab-maximum-indents '(;; = is a convenience. Don't go too far
                                    (?= . (10 . 4))
                                    ;; Fns should provide hard limits
                                    (?\( . 50)
                                    ;; Matrix/Cell arrays
                                    (?\[ . 20)
                                    (?\{ . 20))
  "Alist of maximum indentations when lining up code.
Each element is of the form (CHAR . INDENT) where char is a character
the indent engine is using, and INDENT is the maximum indentation
allowed.  Indent could be of the form (MAXIMUM . INDENT), where
MAXIMUM is the maximum allowed calculated indent, and INDENT is the
amount to use if MAXIMUM is reached."
  :group 'matlab
  :type '(repeat (cons (character :tag "Open List Character")
                       (sexp :tag "Number (max) or cons (max indent)"))))

(defcustom matlab-align-to-paren t
  "*Whether continuation lines should be aligned to the opening parenthesis.
When non-nil, continuation lines are aligned to the opening parenthesis if the
opening is not followed by only spaces and ellipses.  When nil, continued lines
are simply indented by `matlab-continuation-indent-level'."
  :group 'matlab
  :type 'boolean
  )

(defcustom matlab-fill-fudge 10
  "Number of characters around `fill-column' we can fudge filling.
Basically, there are places that are very convenient to fill at, but
might not be the closest fill spot, or occur after `fill-column'.
If they occur within this fudge factor, we will use them.
Also, if none of the above occur, and we find a symbol to break at,
but an open paren (group) starts or ends within this fudge factor,
move there to boost the amount of fill leverage we can get."
  :group 'matlab
  :type 'integer)

(defcustom matlab-fill-fudge-hard-maximum 79
  "The longest line allowed when auto-filling code.
This overcomes situations where the `fill-column' plus the
`matlab-fill-fudge' is greater than some hard desired limit."
  :group 'matlab
  :type 'integer)

(defcustom matlab-elipsis-string "..."
  "Text used to perform continuation on code lines.
This is used to generate and identify continuation lines."
  :group 'matlab
  :type 'string)

(defcustom matlab-fill-code nil
  "*If true, `auto-fill-mode' causes code lines to be automatically continued."
  :group 'matlab
  :type 'boolean)

(defcustom matlab-fill-count-ellipsis-flag t
  "*Non-nil means to count the ellipsis when auto filling.
This effectively shortens the `fill-column' by the length of
`matlab-elipsis-string'."
  :group 'matlab
  :type 'boolean)

(defcustom matlab-fill-strings-flag t
  "*Non-nil means that when auto-fill is on, strings are broken across lines.
If `matlab-fill-count-ellipsis-flag' is non nil, this shortens the
`fill-column' by the length of `matlab-elipsis-string'."
  :group 'matlab
  :type 'boolean)

(defcustom matlab-comment-column 40
  "*The goal comment column in `matlab-mode' buffers."
  :group 'matlab
  :type 'integer)

(defcustom matlab-comment-anti-indent 0
  "*Amount of anti-indentation to use for comments in relation to code."
  :group 'matlab
  :type 'integer)

(defcustom matlab-comment-line-s "% "
  "*String to start comment on line by itself."
  :group 'matlab
  :type 'string)

(defcustom matlab-comment-on-line-s "% "
  "*String to start comment on line with code."
  :group 'matlab
  :type 'string)

(defcustom matlab-comment-region-s "% $$$ "
  "*String inserted by \\[matlab-comment-region] at start of each line in \
region."
  :group 'matlab
  :type 'string)

(defcustom matlab-mode-hook nil
  "*List of functions to call on entry to MATLAB mode."
  :group 'matlab
  :type 'hook)

(defcustom matlab-change-current-directory nil
  "*If non nil, make file's directory the current directory when evaluating it."
  :group 'matlab
  :type 'boolean)

(make-variable-buffer-local 'matlab-change-current-directory)

(defvar matlab-mode-abbrev-table nil
  "The abbrev table used in `matlab-mode' buffers.")
(define-abbrev-table 'matlab-mode-abbrev-table ())


;;; Keybindings ===============================================================

(defvar matlab-help-map
  (let ((km (make-sparse-keymap)))
    (define-key km "r" 'matlab-shell-run-command)
    (define-key km "f" 'matlab-shell-describe-command)
    (define-key km "a" 'matlab-shell-apropos)
    (define-key km "v" 'matlab-shell-describe-variable)
    km)
  "The help key map for `matlab-mode' and `matlab-shell-mode'.")

;; mode map
(defvar matlab-mode-map
  (let ((km (make-sparse-keymap)))
    ;; Navigation Commands
    (define-key km [(meta a)] 'matlab-beginning-of-command)
    (define-key km [(meta e)] 'matlab-end-of-command)
    ;; Insert, Fill stuff
    (define-key km [(control c) (control c)] 'matlab-insert-map-fcn)
    (define-key km [(control c) (control f)] 'matlab-fill-comment-line)
    (define-key km [(control c) (control j)] 'matlab-justify-line)
    (define-key km [(control c) (control q)] 'matlab-fill-region)
    ;; Comment Stuff
    (define-key km "\C-c;" 'matlab-comment-region)
    (define-key km "\C-c:" 'matlab-uncomment-region)
    (define-key km [(meta \;)] 'matlab-comment)
    (define-key km [(meta j)] 'matlab-comment-line-break-function)
    (define-key km [(control c) return] 'matlab-comment-return)
    (substitute-key-definition 'comment-region 'matlab-comment-region
                               km global-map) ;torkel
    ;; Completion
    (define-key km "\M-\t" 'matlab-complete-symbol)
    ;; Connecting to MATLAB Shell
    (define-key km [(control c) (control s)] 'matlab-shell-save-and-go)
    (define-key km [(control c) (control r)] 'matlab-shell-run-region)
    (define-key km [(meta control return)] 'matlab-shell-run-cell)
    (define-key km [(control return)] 'matlab-shell-run-region-or-line)
    (define-key km [(control c) (control t)] 'matlab-show-line-info)
    (define-key km [(control c) ?. ] 'matlab-shell-locate-fcn)
    (define-key km [(control h) (control m)] matlab-help-map)
    (define-key km [(meta s)] 'matlab-show-matlab-shell-buffer)
    (define-key km [(control meta mouse-2)] 'matlab-find-file-click)
    ;; Debugger interconnect
    (substitute-key-definition 'read-only-mode 'matlab-toggle-read-only
                               km global-map)

    km)
  "The keymap used in `matlab-mode'.")

;;; TODO - this menu was all about when emacs didn't always have windows (e18 ?)
;;  turn this into a regular menu definition.
(defvar matlab-mode-menu-keymap nil
  "Keymap used in MATLAB mode to provide a menu.")

;; make a menu keymap
(easy-menu-define matlab-mode-menu matlab-mode-map "MATLAB menu"
  '("MATLAB"
    ["Start MATLAB" matlab-shell
     :active (not (matlab-shell-active-p))
     :visible (not (matlab-shell-active-p)) ]
    ["Switch to MATLAB" matlab-shell
     :active (matlab-any-shell-active-p)
     :visible (matlab-any-shell-active-p)]
    ["Save and go" matlab-shell-save-and-go
     :active (matlab-any-shell-active-p) ]
    ["Run Region" matlab-shell-run-region
     :active (matlab-any-shell-active-p) ]
    ["Run Cell" matlab-shell-run-cell
     :active (matlab-any-shell-active-p) ]
    ["Version" matlab-show-version t]
    "----"
    ["Locate MATLAB function" matlab-shell-locate-fcn
     :active (matlab-shell-active-p)
     :help "Run 'which FCN' in matlab-shell, then open the file in Emacs"]
    ("Debug"
     ["Edit File (toggle read-only)" matlab-shell-gud-mode-edit
      :help "Exit MATLAB debug minor mode to edit without exiting MATLAB's K>> prompt."
      :visible gud-matlab-debug-active ]
     ["Add Breakpoint (ebstop in FILE at point)" mlgud-break
      :active (matlab-shell-active-p)
      :help "When MATLAB debugger is active, set break point at current M-file point"]
     ["Remove Breakpoint (ebclear in FILE at point)" mlgud-remove
      :active (matlab-shell-active-p)
      :help "When MATLAB debugger is active, remove break point in FILE at point." ]
     ["List Breakpoints (ebstatus)" mlgud-list-breakpoints
      :active (matlab-shell-active-p)
      :help "List active breakpoints."]
     ["Step (dbstep in)" mlgud-step
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, step into line"]
     ["Next (dbstep)" mlgud-next
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, step one line"]
     ["Finish function  (dbstep out)" mlgud-finish
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, run to end of function"]
     ["Continue (dbcont)" mlgud-cont
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, run to next break point or finish"]
     ["Evaluate Expression" matlab-shell-gud-show-symbol-value
      :active (matlab-any-shell-active-p)
      :help "When MATLAB is active, show value of the symbol under point."]
     ["Show Stack" mlg-show-stack
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, show the stack in a buffer."]
;;;  Advertise these more if we can get them working w/ mlgud's frame show.
;;;      ["Up Call Stack (dbup)" mlgud-up
;;;       :active gud-matlab-debug-active
;;;       :help "When MATLAB debugger is active and at break point, go up a frame"]
;;;      ["Down Call Stack (dbdown)" mlgud-down
;;;       :active gud-matlab-debug-active
;;;       :help "When MATLAB debugger is active and at break point, go down a frame"]
     ["Quit debugging (dbquit)" mlgud-stop-subjob
      :active gud-matlab-debug-active
      :help "When MATLAB debugger is active, stop debugging"]
     )

    ;; TODO - how to autoload these?  Do we want this menu?
    ;;     ("Insert"
    ;;      ["Complete Symbol" matlab-complete-symbol t]
    ;;      ["Comment" matlab-comment t]
    ;;      ["if end" tempo-template-matlab-if t]
    ;;      ["if else end" tempo-template-matlab-if-else t]
    ;;      ["for end" tempo-template-matlab-for t]
    ;;      ["switch otherwise end" tempo-template-matlab-switch t]
    ;;      ["Next case" matlab-insert-next-case t]
    ;;      ["try catch end" tempo-template-matlab-try t]
    ;;      ["while end" tempo-template-matlab-while t]
    ;;      ["End of block" matlab-insert-end-block t]
    ;;      ["Function" tempo-template-matlab-function t]
    ;;      ["Stringify Region" matlab-stringify-region t]
    ;;      )
    ("Customize"
     ["Customize" (customize-group 'matlab)
      (and (featurep 'custom) (fboundp 'custom-declare-variable))
      ]
     )
    "----"
    ["Run M Command" matlab-shell-run-command (matlab-shell-active-p)]
    ["Describe Command" matlab-shell-describe-command (matlab-shell-active-p)]
    ["Describe Variable" matlab-shell-describe-variable (matlab-shell-active-p)]
    ["Command Apropos" matlab-shell-apropos (matlab-shell-active-p)]
    ))
(easy-menu-add matlab-mode-menu matlab-mode-map)


;;; Font Lock : Character Vectors, Strings and Comments ================================
;;
;; Combine these, but do all the matching internally instead of using regexp
;; because it's just too complex for a regular expression.
(defface matlab-region-face
  '((t :inherit region))
  "*Face used to highlight a matlab region."
  :group 'matlab)

(defvar matlab-unterminated-string-face 'matlab-unterminated-string-face
  "Self reference for unterminated string face.")

(defvar matlab-commanddual-string-face 'matlab-commanddual-string-face
  "Self reference for command dual string face.")

(defvar matlab-simulink-keyword-face 'matlab-simulink-keyword-face
  "Self reference for simulink keywords.")

(defvar matlab-nested-function-keyword-face 'matlab-nested-function-keyword-face
  "Self reference for nested function/end keywords.")

(defvar matlab-cross-function-variable-face 'matlab-cross-function-variable-face
  "Self reference for cross-function variables.")

(defvar matlab-cellbreak-face 'matlab-cellbreak-face
  "Self reference for cellbreaks.")

(defvar matlab-math-face 'matlab-math-face
  "Self reference for math.")

(defface matlab-unterminated-string-face
  '((t :inherit font-lock-string-face
       :underline t))
  "*Face used to highlight unterminated strings."
  :group 'matlab)

(defface matlab-commanddual-string-face
  '((t :inherit font-lock-string-face
       :slant italic))
  "*Face used to highlight command dual string equivalent."
  :group 'matlab)

(defface matlab-simulink-keyword-face
  '((t :inherit font-lock-builtin-face
       :underline t))
  "*Face used to highlight simulink specific functions."
  :group 'matlab)

(defface matlab-nested-function-keyword-face
  '((t :inherit font-lock-keyword-face
       :slant  italic))
  "*Face to use for cross-function variables.")

(defface matlab-cross-function-variable-face
  '((t :weight bold
       :slant  italic))
  "*Face to use for cross-function variables."
  :group 'matlab)

(defface matlab-cellbreak-face
  '((t :inherit font-lock-comment-face
       :overline t
       :bold t))
  "*Face to use for cellbreak %% lines.")

(defface matlab-ignored-comment-face
  '((t :inherit font-lock-comment-face
       :slant italic))
  "*Face to use for ignored comments.
Ignored comments are lines that start with '% $$$'  or '%^'.")

(defface matlab-pragma-face
  '((t :inherit font-lock-comment-face
       :bold t))
  "*Face to use for cellbreak %% lines.")

(defface matlab-math-face
  '((t :inherit font-lock-constant-face
       :slant italic))
  "*Face to use for cellbreak %% lines.")

(defcustom matlab-hg-primitives-list
  '(;; start with basic / primitive objects
    "figure" "axes" "line" "surface" "patch" "text" "light" "image" "imagesc"
    "rectangle" "animatedline"
    ;; core utilities
    "set" "get" "reset" "copyobj" "findobj" "cla" "clf" "shg"
    ;; popular helpers
    "axis" "hold" "title" "xlabel" "ylabel" "zlabel" "xlim" "ylim" "zlim" "rlim" "thetalim"
    "lighting" "shading" "material"
    ;; popular cartesian charts
    "plot" "plot3" "semilogx" "semilogy" "loglog" "scatter" "scatter3" "stackedplot"
    "area" "errorbar" "bubblechart" "bubblechart3" "swarmchart" "swarmchart3" "spy"
    "histogram" "histogram2" "wordcloud" "bubblecloud" "heatmap" "parallelplot"
    "bar" "barh" "bar3" "bar3h" "stem" "stairs" "quiver" "quiver3" "stem3"
    "contour" "contourf" "contour3" "contourslice" "fcontour"
    ;; 3D
    "surf" "surfc" "surfl" "ribbon" "pcolor" "mesh" "meshc" "meshz" "waterfall"
    ;; anim
    "comet" "comet3"
    ;; polar
    "polarplot" "polarscatter" "polarhistogram" "polarbubblechart"
    ;; geographic
    "goeplot" "geoscatter" "geobubble" "geodensity"
    ;; function plots
    "fplot" "fplot3" "fimplicit" "fsurf" "fimplicit3"
    ;; misc tools
    "legend" "colorbar" "tiledlayout" "nexttile" "subplot" "annotation"
    ;; Components
    "uicontrol" "uimenu" "uitoolbar" "uitoggletool" "uipushtool" "uicontext" "uicontextmenu"
    ;; misc dialogs
    "uisetfont" "uisetcolor" "uigetfile" "uiputfile")

  "List of handle graphics functions used in highlighting.
Customizing this variable is only useful if `regexp-opt' is available."
  :group 'matlab
  :type '(repeat (string :tag "HG Keyword: ")))

(defcustom matlab-debug-list '("dbstop" "dbclear" "dbcont" "dbdown" "dbmex"
                               "dbstack" "dbstatus" "dbstep" "dbtype" "dbup"
                               "dbquit")
  "List of debug commands used in highlighting.
Customizing this variable is only useful if `regexp-opt' is available."
  :group 'matlab
  :type '(repeat (string :tag "Debug Keyword: ")))

(defcustom matlab-simulink-keywords
  '("simulink" "get_param" "set_param" "simget" "simset" "sim"
    "new_system" "open_system" "close_system" "save_system" "find_system"
    "add_block" "delete_block" "replace_block"
    "add_line" "delete_line" "replace_line"
    "bdroot" "bdclose" )
  ;; Missing this regex "\\(mld\\|ss\\)[A-Z]\\w+\\)"
  "List of keywords to highlight for simulink."
  :group 'matlab
  :type '(repeat (string :tag "Debug Keyword: ")))


(defcustom matlab-constants-keyword-list
  '("eps" "pi" "flintmax" "inf" "Inf" "nan" "NaN" "ans" "i" "j" "NaT" "true" "false")
  "List of constants and special variables in MATLAB."
  :group 'matlab
  :type '(repeat (string :tag "Debug Keyword: ")))

(defun matlab-font-lock-regexp-opt (keywordlist)
  "Create a font-lock usable KEYWORDLIST matching regular expression.
Uses `regex-opt' if available.  Otherwise creates a 'dumb' expression."
  (concat "\\_<\\("
          (if (fboundp 'regexp-opt)
              (regexp-opt keywordlist)
            (mapconcat (lambda (s) s) keywordlist "\\|"))
          "\\)\\_>"))

;;; Font lock keyword handlers
(defvar font-lock-beg) (defvar font-lock-end) ; quiet compiler.

(defun matlab-font-lock-extend-region ()
  "Called by font-lock to extend the region for multiline expressions.
Supports expressions like arguments and property blocks with anchored
color support."
  (save-excursion
    (let* ((flb font-lock-beg)
           (fle font-lock-end)
           (tmp (matlab--scan-block-backward-up (window-start)))
           (blockmatch (when (not tmp) (matlab--mk-keyword-node))))
      (when (and (member (nth 1 blockmatch) '("properties" "events" "arguments"))
                 (matlab--valid-keyword-node blockmatch))
        (setq font-lock-beg (min font-lock-beg (point-at-bol)))
        (when (not (matlab--scan-next-keyword 'all (window-end)))
          (setq font-lock-end (max font-lock-end (point-at-eol)))))

      (if (and (eq font-lock-beg flb)
               (eq font-lock-end fle))
          ;; We didn't change anything.
          nil

        ;; We made a change
        t))))

(defvar ml-fl-anchor-limit nil)
(defun matlab-font-lock-anchor-set-end-limit ()
  "Set the end limit for anchored matchers."
  (save-excursion
    (save-match-data
      ;; next keyword is faster, plus if someone is in the middle of typing
      ;; a new block, prevents going too far into the distance.
      (matlab--scan-next-keyword 'all (point-max))
      (forward-word -1)
      (setq ml-fl-anchor-limit (point)))))

(defun matlab-font-lock-anchor-clear-end-limit ()
  "Clear the end limit for anchored matchers."
  (setq ml-fl-anchor-limit nil))

;;; Font Lock keyword handling
;;
;; Many parts of the keyword handling are shared with matlab-shell.
;; The matlab based variables here are divided up between generic keywords
;; and keywords only for M files.  This means the M shell won't highlight
;; some syntaxes like classdef stuff even though someone might paste them in.
;;
;; matlab-*-keywords      -- MATLAB Files or Shell
;; matlab-file-*-keywords -- MATLAB Files only

(defconst matlab-basic-font-lock-keywords
  (list
   ;; Handle graphics stuff
   (list
    (matlab-font-lock-regexp-opt matlab-hg-primitives-list)
    '(0 font-lock-builtin-face))
   (list
    ;; How about a few matlab constants such as pi, infinity, and sqrt(-1)?
    (matlab-font-lock-regexp-opt matlab-constants-keyword-list)
    1 'matlab-math-face)
   ;; Imaginary number support
   '("\\<[0-9]\\.?\\(i\\|j\\)\\_>" 1 font-lock-reference-face)
   )
  "Basic Expressions to highlight in MATLAB mode or shell.")

(defconst matlab-file-basic-font-lock-keywords
   matlab-basic-font-lock-keywords
  "Basic Expressions to highlight in MATLAB Files.")

(defconst matlab-fl-opt-continuation "\\s<\\S>+\\s>")
(defconst matlab-fl-opt-whitespace (concat "\\s-*\\(?:"
                                           matlab-fl-opt-continuation
                                           "\\)?\\s-*"))

(defconst matlab-fl-fcn-key "^\\s-*function\\_>")
(defconst matlab-fl-return-args "\\(\\[[^]]*\\]\\|\\sw+\\)")
(defconst matlab-fl-fcn-name "\\(?:[sg]et\\.\\)?\\sw+")
(defconst matlab-fl-fcn-args "\\(?:(\\|$\\|\\s<\\)" )

(defconst matlab-function-font-lock-keywords
  (list
   ;; defining a function, a (possibly empty) list of assigned variables,
   ;; function name, and an optional (possibly empty) list of input variables
   (list (concat matlab-fl-fcn-key matlab-fl-opt-whitespace
                 matlab-fl-return-args matlab-fl-opt-whitespace
                 "=" matlab-fl-opt-whitespace
                 "\\(" matlab-fl-fcn-name "\\)" matlab-fl-opt-whitespace
                 matlab-fl-fcn-args)
         '(1 font-lock-variable-name-face append)
         '(2 font-lock-function-name-face prepend))
   ;; defining a function, a function name, and an optional (possibly
   ;; empty) list of input variables
   (list (concat matlab-fl-fcn-key matlab-fl-opt-whitespace
                 "\\(" matlab-fl-fcn-name "\\)" matlab-fl-opt-whitespace
                 matlab-fl-fcn-args)
         '(1 font-lock-function-name-face prepend))
   ;; Anchor on the function keyword, highlight params
   (list (concat matlab-fl-fcn-key matlab-fl-opt-whitespace
                 "\\(" matlab-fl-return-args matlab-fl-opt-whitespace
                 "=" matlab-fl-opt-whitespace
                 "\\)?"
                 matlab-fl-fcn-name matlab-fl-opt-whitespace
                 "(")
         (list (concat matlab-fl-opt-whitespace "\\(\\sw+\\)"
                       matlab-fl-opt-whitespace "[,)]")
               '(save-excursion
                  (condition-case nil
                      (matlab-scan-end-of-command)
                    (error (point-at-eol))))
               nil
               '(1 font-lock-variable-name-face)))
   ;; I like variables for FOR loops
   '("\\<\\(\\(?:par\\)?for\\)\\s-+\\(\\sw+\\)\\s-*=\\s-*\
\\(\\([^\n,;%(]+\\|([^\n%)]+)\\)+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face append)
     (3 font-lock-reference-face append))
   ;; Items after a switch statements are cool
   '("\\_<\\(case\\|switch\\)\\_>\\s-+\\({[^}\n]+}\\|[^,%\n]+\\)"
     (2 font-lock-reference-face))
   ;; set_param and waitfor have input variables that can be highlighted.
   (list (concat matlab-indent-past-arg1-functions "\\s-*")
         '("(\\s-*\\(\\(?:\\w\\|\\.\\)+\\)\\s-*\\(,\\|)\\)" nil  nil
           (1 font-lock-variable-name-face)))
   )
  "List of font lock keywords for stuff in functions.")

(defconst matlab-class-attributes-list-re
  "\\s-*\\(?2:(\\([^)]+\\))\\|\\)"
  "Regular expression for matching an attributes block.")

(defconst matlab-file-class-font-lock-keywords
  (list
   ;; Classdefs keyword and the class name
   (list (concat "^\\s-*\\(classdef\\)\\_>"
                 matlab-class-attributes-list-re
                 "\\s-*\\(?3:\\sw+\\)")
         ;; '(1 font-lock-keyword-face append) - handled as keyword
         '(3 font-lock-function-name-face)
         )
   ;; Classdef anchor for highlighting all the base classes in inherits from
   (list (concat "^\\s-*\\(classdef\\)"
                 matlab-class-attributes-list-re
                 "\\s-+\\(\\sw+\\)")
         '("\\s-*[<&]\\s-*\\(\\(\\sw\\|\\.\\)+\\)" nil  nil
           (1 font-lock-constant-face)))
   ;; Property and Method blocks have attributes to highlight
   (list "^\\s-*\\(classdef\\|properties\\|methods\\|events\\|arguments\\)\\s-*("
         '("\\(\\sw+\\)\\s-*\\(=\\s-*[^,)]+\\)?" nil  nil
           (1 font-lock-type-face)
           ))
   )
  "List of font-lock keywords used when an MATLAB file contains a class.")

(defconst matlab-file-gaudy-font-lock-keywords
  (append
   matlab-basic-font-lock-keywords
   matlab-file-basic-font-lock-keywords
   matlab-function-font-lock-keywords
   matlab-file-class-font-lock-keywords
   )
  "Expressions to highlight in MATLAB mode.")

(defconst matlab-really-gaudy-font-lock-keywords
  (append
   (list
    ;; Since it's a math language, how bout dem symbols?
    '("\\([<>~=]=\\|\\.[/\\*^'?]\\|\\_<\\(?:\\<xor\\|any\\|all\\|find\\)\\_>\\|[-<>!?^&|*+\\/~:@]\\)"
      1 font-lock-builtin-face)
    ;; highlight transpose
    '("[]A-Za-z0-9_\"})']\\('+\\)" 1 font-lock-builtin-face)
    ;; How about references in the HELP text.
    (list (concat "^" matlab-comment-line-s "\\s-*"
                  "\\(\\([A-Z]+\\s-*=\\s-+\\|\\[[^]]+]\\s-*=\\s-+\\|\\)"
                  "\\([A-Z][0-9A-Z]+\\)\\(([^)\n]+)\\| \\)\\)")
          '(1 font-lock-reference-face prepend))
    (list (concat "^" matlab-comment-line-s "\\s-*"
                  "See also\\s-+")
          '("\\([A-Z][A-Z0-9]+\\)\\([,.]\\| and\\|$\\) *" nil  nil
            (1 font-lock-reference-face prepend)))
    (list (concat "^" matlab-comment-line-s "\\s-*"
                  "\\(\\$" "Revision" "[^\n$]+\\$\\)")
          '(1 font-lock-reference-face prepend))
    ;; Debugging Keywords
    (list (matlab-font-lock-regexp-opt matlab-debug-list)
          '(0 'bold))
    ;; Simulink functions
    (list (matlab-font-lock-regexp-opt matlab-simulink-keywords)
          1 matlab-simulink-keyword-face)
    ))
  "Expressions to highlight in MATLAB mode.")

(defconst matlab-file-really-gaudy-font-lock-keywords
  (append
   matlab-file-gaudy-font-lock-keywords
   matlab-really-gaudy-font-lock-keywords
   )
  "Expressions to highlight in MATLAB mode.")

;; Imenu support.
(defvar matlab-imenu-generic-expression
  '((nil "^\\s-*function\\>[ \t\n.]*\\(\\(\\[[^]]*\\]\\|\\sw+\\)[ \t\n.]*\
< =[ \t\n.]*\\)?\\([a-zA-Z0-9_]+\\)" 3))
  "Expressions which find function headings in MATLAB M files.")


;;; MATLAB mode entry point ==================================================

;; Choose matlab-mode if when loading MATLAB *.m files
;; See "How Emacs Chooses a Major Mode"
;;    https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto-Major-Mode.html

;;;###autoload
(defun matlab-is-matlab-file ()
  "Enter `matlab-mode' when file content looks like a MATLAB *.m
file or for empty files *.m files when `matlab-mode-for-new-mfiles'
indicates as such."
  (and buffer-file-name ;; have a file?
       ;; AND a valid MATLAB file name
       (string-match
        "^\\(?:.*/\\)?[a-zA-Z][a-zA-Z0-9_]*\\.m\\'"  ;; /path/to/file.m ?
        (file-name-sans-versions
         (if (and (boundp 'archive-subfile-mode) archive-subfile-mode)
             (aref archive-subfile-mode 0)   ;; Will just be file.m without the directory
           buffer-file-name)))
       ;; AND (have MATLAB code OR an empty file that should enter matlab-mode)
       (or
        ;; Is content MATLAB code? We can definitely identify *some* MATLAB content using
        ;;    (looking-at "^[[:space:]\n]*\\(%\\|function\\|classdef\\)")
        ;; i.e. '%', '%{' comments, or function/classdef start, but this fails to find MATLAB
        ;; scripts. Thus, if buffer is NOT Objective-C and has something in it, we assume MATLAB.
        ;; Objective-c is identified by
        ;;   - comment start chars: // or /*,
        ;;   - # char (as in #import)
        ;;   - @ char (as in @interface)
        ;; MATLAB scripts are identified by the start of a valid identifier, i.e. a letter or
        ;; some math operation, e.g. [1,2,3]*[1,2,3]', thus all we really need to look for
        ;; is a non-whitespace character which could be a MATLAB comment, generic MATLAB commands,
        ;; function/classdef, etc.
        (and (not (looking-at "^[[:space:]\n]*\\(//\\|/\\*\\|#\\|@\\)"))
             (looking-at "^[[:space:]\n]*[^[:space:]\n]"))
        ;; Empty file - enter matlab-mode based on `matlab-mode-for-new-mfiles' setting
        (and (= (buffer-size) 0)
             (or (equal matlab-mode-for-new-mfiles t)
                 (and (equal matlab-mode-for-new-mfiles 'maybe)
                      ;; Enter matlab-mode if we already have a buffer in matlab-mode
                      (let ((buffers (buffer-list))
                            enter-matlab-mode)
                        (while buffers
                          (with-current-buffer (car buffers)
                            (when (or (eq major-mode 'matlab-mode)
                                      (eq major-mode 'matlab-shell-mode))
                              (setq enter-matlab-mode t)
                              (setq buffers nil)))
                          (setq buffers (cdr buffers)))
                        enter-matlab-mode)))))))

;;;###autoload
(add-to-list 'magic-mode-alist '(matlab-is-matlab-file . matlab-mode))

;;;###autoload
(define-derived-mode matlab-mode prog-mode "MATLAB"
  "MATLAB(R) mode is a major mode for editing MATLAB dot-m files.
\\<matlab-mode-map>
Convenient editing commands are:
 \\[matlab-comment-region]   - Comment/Uncomment out a region of code.
 \\[matlab-fill-comment-line] - Fill the current comment line.
 \\[matlab-fill-region] - Fill code and comments in region.
 \\[matlab-complete-symbol]   - Symbol completion of matlab symbols\
based on the local syntax.

Convenient navigation commands are:
 \\[matlab-beginning-of-command]   - Move to the beginning of a command.
 \\[matlab-end-of-command]   - Move to the end of a command.

Convenient template insertion commands:
 \\[tempo-template-matlab-function] - Insert a function definition.
 \\[tempo-template-matlab-if] - Insert an IF END block.
 \\[tempo-template-matlab-for] - Insert a FOR END block.
 \\[tempo-template-matlab-switch] - Insert a SWITCH END statement.
 \\[matlab-insert-next-case] - Insert the next CASE condition in a SWITCH.
 \\[matlab-insert-end-block] - Insert a matched END statement.  With \
optional ARG, reindent.
 \\[matlab-stringify-region] - Convert plain text in region to a string \
with correctly quoted chars.

Variables:
  `matlab-indent-level'         Level to indent blocks.
  `matlab-continuation-indent-level' Level to indent after ... continuation
  `matlab-case-indent-level'            Level to unindent case statements.
  `matlab-indent-past-arg1-functions'
                                Regexp of functions to indent past the first
                                  argument on continuation lines.
  `matlab-maximum-indents'      List of maximum indents during lineups.
  `matlab-comment-column'       Goal column for on-line comments.
  `fill-column'                 Column used in auto-fill.
  `matlab-fill-code'            Non-nil, auto-fill code in auto-fill-mode.
  `matlab-fill-strings'         Non-nil, auto-fill strings in auto-fill-mode.
  `matlab-verify-on-save-flag'  Non-nil, enable code checks on save.
  `matlab-vers-on-startup'      If t, show version on start-up.
  `matlab-handle-simulink'      If t, enable simulink keyword highlighting.

All Key Bindings:
\\{matlab-mode-map}"

  (use-local-map matlab-mode-map)
  (setq major-mode 'matlab-mode)
  (setq mode-name "MATLAB")
  (if (boundp 'whitespace-modes)
      (add-to-list 'whitespace-modes 'matlab-mode))
  (setq local-abbrev-table matlab-mode-abbrev-table)

  ;; Syntax tables and related features are in matlab-syntax.el
  ;; This includes syntax table definitions, misc syntax regexps
  ;; and font-lock for comments/strings.
  (matlab-syntax-setup)

  ;; Indentation setup.
  (defcustom matlab-indent-offset 4
  "Amount of offset to apply using tree-sitter-indents"
  :group 'matlab
  :type 'integer)
  (defcustom tree-sitter-indent-matlab-scopes
    '((indent-all . ;; these nodes are always indented
                  ())
      (indent-rest . ;; if parent node is one of this and node is not first → indent
                   ())
      (indent-body . ;; if parent node is one of this and current node is in middle → indent
                   (class_definition
                    arguments_statement
                    function_definition
                    properties
                    methods
                    for_statement
                    if_statement
                    switch_statement
                    try_statement
                    while_statement))

      (paren-indent . ;; if parent node is one of these → indent to paren opener
                    (matrix
                     cell
                     strings
                     arguments
                     assignment))
      (align-char-to . ;; chaining char → node types we move parentwise to find the first chaining char
                     ())
      (aligned-siblings . ;; siblings (nodes with same parent) should be aligned to the first child
                        ())

      (multi-line-text . ;; if node is one of this, then don't modify the indent
                       ;; this is basically a peaceful way out by saying "this looks like something
                       ;; that cannot be indented using AST, so best I leave it as-is"
                       ())
      (outdent . ;; these nodes always outdent (1 shift in opposite direction)
               (elseif_clause
                else_clause)))
    "Scopes for indenting in MATLAB"
    :type 'sexp)

  ;; give each file it's own parameter history
  (make-local-variable 'matlab-shell-save-and-go-history)

  ;; Font lock support:
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((matlab-file-gaudy-font-lock-keywords
                              matlab-file-really-gaudy-font-lock-keywords
                              )
                             nil ; use syntax table comments/strings
                             nil ; keywords are case sensitive.
                             ;; This puts _ as a word constituent,
                             ;; simplifying our keywords significantly
                             ((?_ . "w"))))
  (setq font-lock-multiline 'undecided)
  (add-to-list 'font-lock-extend-region-functions #'matlab-font-lock-extend-region t)
  )
;; Support debug mode and read only toggling.
(defvar gud-matlab-debug-active nil)
(declare-function matlab-shell-gud-minor-mode "matlab-shell-gud")

(defun matlab-toggle-read-only (&optional arg interactive)
  "Toggle read-only bit in MATLAB mode.
This looks to see if we are currently debugging, and if so re-enable
our debugging feature.
Optional argument ARG specifies if the read-only mode should be set.
INTERACTIVE is ignored."
  (interactive "P")
  (if (and (featurep 'matlab-shell-gud)
           gud-matlab-debug-active)
      ;; The debugging is active, just re-enable debugging read-only-mode
      (matlab-shell-gud-minor-mode 1)
    ;; Else - it is not - probably doing something else.
    (call-interactively 'read-only-mode)
    ))


;;; Utilities =================================================================

(defun matlab-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "matlab-mode, version %s" matlab-mode-version))

(provide 'matlab)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; matlab.el ends here

;; LocalWords:  Wette mwette edu Ludlam eludlam defconst compat easymenu defcustom mfiles objc elec
;; LocalWords:  CASEINDENT COMMANDINDENT sexp sg Fns Alist symbolp defun mmode setq decl memq progn
;; LocalWords:  elipsis vf functionname booleanp keymap torkel fboundp gud ebstop mlgud ebclear
;; LocalWords:  ebstatus mlg mlgud's subjob featurep defface commanddual cellbreak cellbreaks cdr
;; LocalWords:  animatedline rlim thetalim cartesian stackedplot bubblechart swarmchart wordcloud
;; LocalWords:  bubblecloud heatmap parallelplot fcontour anim polarplot polarscatter polarhistogram
;; LocalWords:  polarbubblechart goeplot geoscatter geobubble geodensity fimplicit fsurf tiledlayout
;; LocalWords:  nexttile uicontext mld flintmax keywordlist mapconcat vardecl flb fle blockmatch bol
;; LocalWords:  eol tm newmdata Classdefs dem Imenu imenu boundp alist reindent unindent vers Sexp's
;; LocalWords:  Defuns fn minibuffer eobp autoend noerror returnme Unstarted parentblock defuns bobp
;; LocalWords:  noprogress minibufferp bolp eolp calc funcall ci sem prevcmd DEPTHNUMBER blockstart
;; LocalWords:  blockmid blockendless blockend CTXT listp fc pc boc parencol parenchar parenpt
;; LocalWords:  parenindent parenopt FUNCTIONs MAXs prev startpnt depthchange bc emacsen afterd
;; LocalWords:  befored okpos startlst endlst ellipsify noreturn hs tc hc startsym endsym mapc func
;; LocalWords:  filetype bn nondirectory scanstate sexp's nosemi msgpos fullindent nexti defn
;; LocalWords:  classdef's
