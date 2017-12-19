;; cpr-rt.el --- citeproc-el rich-text functions -*- lexical-binding: t; -*-

;; Copyright (C) 2017 András Simonyi

;; Author: András Simonyi <andras.simonyi@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Functions operating on rich-text contents. In citeproc-el, rich-texts are
;; represented either by strings or by lists of the form (ATTRS RT_1 RT_2...)
;; where ATTRS is an alist consisting of (FORMAT-ATTR . VALUE) pairs and RT_1,
;; RT_2... are all rich-texts. The constants `cpr-rt-format-attrs' and
;; `cpr-rt-ext-format-attrs' define the list of normal and extended format
;; attributes, respectively. As a degenerative case, nil is also a legitimate
;; rich-text.

;;; Code:

(require 'subr-x)
(require 'dash)
(require 'cl-lib)
(require 'let-alist)

(require 'cpr-s)
(require 'cpr-lib)

(defconst cpr-rt-format-attrs
  '(font-variant font-style font-weight text-decoration vertical-align font-variant
		 display rendered-var name-id quotes cited-item-no bib-item-no
		 rendered-names)
  "The rich-text content format attributes (used in raw output).")

(defconst cpr-rt-ext-format-attrs
  (-concat '(prefix suffix delimiter subst quotes) cpr-rt-format-attrs)
  "The list of extended format attributes.")

(defun cpr-rt-to-plain (rt)
  "Return the plain-text content of rich-text RT."
  (if (listp rt)
      (mapconcat 'cpr-rt-to-plain
		 (cdr rt)
		 "")
    rt))

(defun cpr-rt-attrs (rt)
  "Return the attrs of rich content RT."
  (if (listp rt) (car rt) nil))

(defun cpr-rt-first-content (rt)
  "Return the first content element of rich content RT."
  (if (listp rt) (cadr rt) rt))

(defun cpr-rt-select-attrs (attrs keep)
  "Select attr-val pairs from alist ATTRS whose attr is in KEEP."
  (--filter (memq (car it) keep)
	    attrs))

(defun cpr-rt-join-strings (rt)
  "Concatenate consecutive strings in rich-text RT."
  (cond ((< (length rt) 2)
	 rt)
	((and (char-or-string-p (car rt))
	      (char-or-string-p (cadr rt)))
	 (cpr-rt-join-strings (cons (concat (car rt)
					    (cadr rt))
				    (cddr rt))))
	(t (cons (car rt)
		 (cpr-rt-join-strings (cdr rt))))))

(defun cpr-rt-splice-unformatted (rt)
  "Splice the body of its unformatted elements into rich-text RT."
  (if (and (consp rt) (not (alist-get 'delimiter (car rt))))
      (cons (car rt)
	    (--mapcat (if (cpr-rt-formatting-empty-p it)
			  (cdr it)
			(list it))
		      (cdr rt)))
    rt))

(defun cpr-rt-formatting-empty-p (rt)
  "Is the formatting of rich-text RT empty/redundant?"
  (and (consp rt) (or (not (caar rt))
		      (and (eq (cl-caaar rt) 'delimiter)
			   (= (length (car rt)) 1)
			   (= (length rt) 2)))))

(defun cpr-rt-reduce-content (rt)
  "Reduce rich-text RT if it has no attributes or body.
Return the original RT if it has non-empty attrs and content."
  (cond ((not (cdr rt)) nil)
	((and (not (car rt)) (= (length rt) 2)) (cadr rt))
	(t rt)))

(defun cpr-rt-simplify-shallow (rt)
  "Simplify the first level of rich-text RT."
  (cpr-rt-reduce-content (cpr-rt-join-strings
			  (cpr-rt-splice-unformatted rt))))

(defun cpr-rt-simplify-deep (rt)
  "Simplify all levels of rich-text RT."
  (if (not (consp rt)) rt
    (-let* (((attrs . conts) rt)
	    (simplifieds (--map (cpr-rt-simplify-deep it)
				conts)))
      (cpr-rt-reduce-content
       (cpr-rt-join-strings
	(cpr-rt-splice-unformatted (cons attrs simplifieds)))))))

(defun cpr-rt-map-strings (fun rts)
  "Map through FUN all strings in rich-texts RTS.
Return a new rich-text with all S content strings replaced by the
value of FUN applied to S. No formatting is changed."
  (--map (cpr-rt-format it fun) rts))

(defun cpr-rt-format (rt fun)
  "Format all plain text in RT with FUN."
  (if (listp rt)
      (cons (car rt) (cpr-rt-map-strings fun (cdr rt)))
    (funcall fun rt)))

(defun cpr-rt-replace-all (replacements rts)
  "Make all REPLACEMENTS in the strings if rich-texts RTS."
  (cpr-rt-map-strings (lambda (x) (s-replace-all replacements x))
		      rts))

(defun cpr-rt-strip-periods (rts)
  "Remove all periods from rich-texts RTS."
  (cpr-rt-replace-all `(("." . "")) rts))

(defun cpr-rt--update-from-plain-1 (rt p start)
  "Update rich-text RT from plain text P from position START in P.
The length of the plain text content of RT must not be less than
the length of P. Return an (UPDATED . NEXT) pair where UPDATED is
the updated rich-text and NEXT is the first position in P which
was not used for the update."
  (if (listp rt)
      (let ((act-start start))
	(cons (cons (car rt)
		    (--map (-let (((updated . next)
				   (cpr-rt--update-from-plain-1
				    it p act-start)))
			     (setq act-start next)
			     updated)
			   (cdr rt)))
	      act-start))
    (let ((end (+ start (length rt))))
      (cons (substring p start end) end))))

(defun cpr-rt-update-from-plain (rt p)
  "Update rich-text RT from plain text P.
The length of the plain text content of RT must not be less than
the length of P. Return the updated rich-text."
  (car (cpr-rt--update-from-plain-1 rt p 0)))

(defun cpr-rt-change-case (rt case-fun)
  "Change the case of rich text RT with CASE-FUN.
CASE-FUN is a function taking a string as its argument and
returning a string of the same length."
  (let ((plain (cpr-rt-to-plain rt)))
    (cpr-rt-update-from-plain rt (funcall case-fun plain))))

(defun cpr--textcased (rts case)
  "Return rich-text content RTS in text-case CASE.
CASE is one of the following: 'lowercase, 'uppercase,
'capitalize-first, 'capitalize-all, 'sentence, 'title."
  (pcase case
    ('uppercase
     (cpr-rt-map-strings #'upcase rts))
    ('lowercase
     (cpr-rt-map-strings #'downcase rts))
    ('capitalize-first
     (--map (cpr-rt-change-case it #'cpr-s-capitalize-first) rts))
    ('capitalize-all
     (--map (cpr-rt-change-case it #'cpr-s-capitalize-all) rts))
    ('sentence
     (--map (cpr-rt-change-case it #'cpr-s-sentence-case) rts))
    ('title
     (--map (cpr-rt-change-case it #'cpr-s-title-case) rts))))

(defun cpr-rt-pred-counts-tree (rt pred)
  "Return a dominated count tree for rich text RT based on PRED.
The returned tree has the same structure as RT but the car of
each subtree is a number indicating the maximal number of nodes
on any dominated branch for which PRED holds."
  (if (consp rt)
      (let ((children-vals (--map (cpr-rt-pred-counts-tree it pred)
				  (cdr rt))))
	(cons (-max (cl-mapcar (lambda (rich count)
				 (+ (if (listp count) (car count) count)
				    (if (funcall pred rich) 1 0)))
			       (cdr rt)
			       children-vals))
	      children-vals))
    0))

(defun cpr-rt-flip-italics (rt)
  "Flip the italic attribute of rich text RT."
  (if (listp rt)
      (cons (if (cpr-rt-in-italics-p rt)
		(--remove (eq (car it) 'font-style) (car rt))
	      (cons '(font-style . "italic") (car rt)))
	    (cdr rt))
    `(((font-style . "italic")) ,rt)))

(defun cpr-rt-in-italics-p (rt)
  "Whether rich text RT has italic font style as attribute."
  (if (listp rt)
      (string= (alist-get 'font-style (car rt)) "italic")
    nil))

(defun cpr-rt-italics-flipflop (rt)
  "Return a flipflopped italics version of rich text RT."
  (if (and rt (listp rt))
      (let ((italics-counts-tree (cpr-rt-pred-counts-tree rt 'cpr-rt-in-italics-p)))
	(if (> (+ (car italics-counts-tree)
		  (if (cpr-rt-in-italics-p rt) 1 0))
	       1)
	    (cpr-rt--italics-flipflop-1 rt italics-counts-tree)
	  rt))
    rt))

(defun cpr-rt--italics-flipflop-1 (rt italics-counts-tree)
  "Flipflop italics in RT using info from ITALICS-COUNTS-TREE."
  (let ((rt-italic (cpr-rt-in-italics-p rt)))
    (if (or (not (listp rt))
	    (not (listp italics-counts-tree))
	    (< (+ (car italics-counts-tree)
		  (if rt-italic 1 0))
	       2)) rt
      (if rt-italic
	  (cons (--remove (eq (car it) 'font-style) (car rt))
		(cl-mapcar (lambda (r i)
			     (cpr-rt--italics-flipflop-1 (cpr-rt-flip-italics r) i))
			   (cdr rt)
			   (cdr italics-counts-tree)))
	(cons (car rt)
	      (cl-mapcar (lambda (r i) (cpr-rt--italics-flipflop-1 r i))
			 (cdr rt)
			 (cdr italics-counts-tree)))))))

(defun cpr-rt-from-str (s)
  "Parse a html or plain text string S into rich text."
  (if (and s (s-matches-p "</[[:alnum:]]+>" s))
      (let* ((parsed (cpr-lib-parse-html-frag s))
	     (body (cddr (cl-caddr parsed)))
	     (stripped (if (eq (caar body) 'p) (cl-cddar body) body)))
	(if (= 1 (length stripped))
	    (cpr-rt-from-html (car stripped))
	  (cons nil (mapcar 'cpr-rt-from-html stripped))))
    s))

(defconst cpr-rt-from-html-alist
  '(((i . nil) . (font-style . "italic"))
    ((b . nil) . (font-weight . "bold"))
    ((span . ((style . "font-variant:small-caps;"))) . (font-variant . "small-caps"))
    ((sc . nil) . (font-variant . "small-caps"))
    ((sup . nil) . (vertical-align . "sup"))
    ((sub . nil) . (vertical-align . "sub")))
  "A mapping from html tags and attrs to rich text attrs.")

(defun cpr-rt-from-html (h)
  "Convert simple html H to rich text."
  (if (listp h)
      (cons (if-let (attr (assoc-default (cons (car h) (cadr h)) cpr-rt-from-html-alist))
		(list attr)
	      '(nil))
	    (mapcar #'cpr-rt-from-html (cddr h)))
    h))

(defun cpr-rt--cquote-pstns-1 (rt offset)
  "Return closing quote positions in rich text RT with OFFSET.
The positions are in the plain text of RT and only those
positions are returned which are associated with a CSL
`quotes'=\"yes\" attribute."
  (if (listp rt)
      (let ((inner (let ((act-offset offset)
			 pstns)
		     (--each (cdr rt)
		       (-let (((p . next) (cpr-rt--cquote-pstns-1 it act-offset)))
			 (setq pstns (nconc pstns p)
			       act-offset next)))
		     (cons pstns act-offset))))
	(if (string= (alist-get 'quotes (car rt)) "true")
	    (-let (((inner-pstns . inner-offset) inner))
	      (cons (cons (1- inner-offset) inner-pstns)
		    inner-offset))
	  inner))
    (cons nil (+ offset (length rt)))))

(defun cpr-rt--cquote-pstns (rt)
  "Return a list of closing quote positions in RT.
The positions are in the plain text of RT and only those
positions are returned which are associated with a CSL
`quotes'=\"yes\" attribute. Numbering starts from 1.
The positions are in decreasing order."
  (sort (car (cpr-rt--cquote-pstns-1 rt 1)) '>))

(defun cpr-rt-punct-in-quote (rt)
  "Put commas and periods inside quotes in rich text RT."
  (if-let ((pstns (cpr-rt--cquote-pstns rt)))
      (let ((plain (cpr-rt-to-plain rt)))
	(cpr-rt-update-from-plain
	 rt
	 (with-temp-buffer
	   (insert plain)
	   (dolist (pos pstns)
	     (goto-char (1+ pos))
	     (when (memq (char-after) '(?, ?.))
	       (call-interactively 'transpose-chars)))
	   (buffer-string))))
    rt))

(defun cpr-rt-find-first-node (rt pred)
  "Return the first node of RT for which PRED holds.
Return nil if no such node was found."
  (if (funcall pred rt) rt
    (pcase rt
      ;; process further if internal node with content
      (`(,_ . ,body)
       (let (found)
	 (while (and (not found) body)
	   (setq found (cpr-rt-find-first-node (car body) pred))
	   (pop body))
	 found))
      ;; leaf or node with no content
      (_ nil))))

(defun cpr-rt-transform-first (rt pred transform)
  "Apply TRANSFORM to the first node of RT for which PRED is non-nil.
PRED and TRANSFORM are functions taking a rich-text node as their
sole argument. Return a (RESULT . SUCCESS) pair where RESULT is
the resulting rich-text and SUCCESS is non-nil iff the
transformation was successfully carried out (i.e., a node
satisfying PRED was found)."
  (if (funcall pred rt) (cons (funcall transform rt) t)
    (pcase rt
      ;; process further if internal node with content
      (`(,attrs . ,body)
       (let* (success
	      (new-body
	       (--map (if success it
			(-let (((it-res . it-success)
				(cpr-rt-transform-first it pred transform)))
			  (setq success it-success)
			  it-res))
		      body)))
	 (cons (cons attrs new-body) success)))
      ;; leaf or node with no content
      (_ (cons rt nil)))))

(defun cpr-rt-add-year-suffix (rt ys)
  "Attempt to add year suffix YS to rich-text RT.
Return an (RT . SUCCESS) pair, where RT is the resulting
rich-text, and SUCCESS is non-nil iff the year-suffix has been
successfully added."
  (cl-flet ((rendered-date-var-p
	     (node)
	     (and (consp node)
		  (memq (alist-get 'rendered-var (car node)) cpr--date-vars)))
	    (add-suffix
	     (node)
	     (if (equal (cadr node) "<suppressed-date>")
		 (list (car node) ys)
	       (-snoc node `(((rendered-var . year-suffix)) ,ys)))))
    (cpr-rt-transform-first rt #'rendered-date-var-p #'add-suffix)))

(defun cpr-rt-replace-first-names (rt replacement)
  "Replace RT's first name-var content with REPLACEMENT.
Return an (RT . SUCCESS) pair, where RT is the resulting
rich-text, and SUCCESS is non-nil iff the replacement has been
successful."
  (cl-flet ((rendered-name-var-p
	     (node)
	     (and (consp node)
		  (assoc 'rendered-names (car node))))
	    (replace (_node) replacement))
    (cpr-rt-transform-first rt #'rendered-name-var-p #'replace)))

(defun cpr-rt-count-names (rt)
  "Return a count of the rendered names in RT."
  (if (consp rt)
      (if (alist-get 'name-id (car rt)) 1
	(apply #'+ (mapcar #'cpr-rt-count-names (cdr rt))))
    0))

(defun cpr-rt-cull-spaces-puncts (rt)
  "Remove unnecessary characters from rich text RT."
  (let* ((plain (cpr-rt-to-plain rt))
	 (updated (cpr-rt-update-from-plain rt (cpr-s-cull-spaces-puncts plain))))
    (cpr-rt-format updated
		   (lambda (x) (replace-regexp-in-string "+" "" x)))))

(defun cpr-rt-render-affixes (rt &optional shallow)
  "Render the affixes in rich-text RT.
If SHALLOW is non-nil then render only the affixes for the first
level."
  (if (not (consp rt))
      rt
    (-let* (((attrs . contents) rt)
	    (rendered
	     (if shallow	 ; We do the recursive call depending on SHALLOW
		 contents
	       (-non-nil (--map (cpr-rt-render-affixes it) contents)))))
      (if rendered
	  (let-alist attrs
	    (let ((delimited (if .delimiter
				 (cdr (--mapcat (list .delimiter it) rendered))
			       rendered)))
	      (if (or .suffix .prefix)
		  (let (result
			outer-attrs
			(inner-attrs (cpr-rt-select-attrs attrs cpr-rt-format-attrs)))
		    (when .display ; The display attribute should encompass affixes
		      (setq outer-attrs (list (cons 'display .display))
			    inner-attrs (--remove (eq (car it) 'display) inner-attrs)))
		    (when .suffix (push .suffix result))
		    (push (cons inner-attrs
				delimited)
			  result)
		    (when .prefix (push .prefix result))
		    (cons outer-attrs result))
		(cons (cpr-rt-select-attrs attrs cpr-rt-format-attrs)
		      delimited))))
	nil))))

(defun cpr-rt-dedup (rt)
  "Remove duplicate substituted renderings from content RT."
  (car (cpr-rt--dedup-single rt nil)))

(defun cpr-rt--dedup-single (rt substs)
  "Remove duplicate subst. var renderings from RT.
SUBSTS contains an initial list of vars to be removed. Return
a (<deduplicated content of RT> <substitued vars in RT> <vars in RT>) list."
  (if (not (consp rt))
      (list rt nil nil)
    (-let* (((attrs . cs) rt)
	    ((&alist 'subst subst
		     'rendered-var var)
	     attrs))
      (if (and var (memq var substs))
	  (list nil nil nil)
	(-let (((new-c s v) (cpr-rt--dedup-multi cs substs)))
	  (list (cons (--reject (memq (car it) '(subst rendered-vars)) attrs)
		      new-c)
		(if subst
		    (-concat v (when var (list var)))
		  s)
		(-concat v (if var
			       (list var)
			     nil))))))))

(defun cpr-rt--dedup-multi (cs substs)
  (if cs
      (-let* (((c s1 v1) (cpr-rt--dedup-single (car cs) substs))
	      ((cs s2 v2) (cpr-rt--dedup-multi (cdr cs) (-concat substs s1))))
	(list (cons c cs)
	      (-concat s1 s2)
	      (-concat v1 v2)))
    (list nil nil nil)))

(defun cpr-rt-finalize (rt &optional punct-in-quote)
  "Finalize rich text RT.
If the optional PUNCT-IN-QUOTE is non-nil then put punctuation
inside quotes.

Note: Finalization doesn't include culling, because some
rich-text transformations require the state before culling (e.g.
the replacement of subsequent authors)."
  ;; The first step is to replace the internally used `modifier letter
  ;; apostrophe' characters with the normal `right single quotation marks'
  (cpr-rt-format (cpr-rt-simplify-deep
		  (cpr-rt-italics-flipflop
		   (if punct-in-quote (cpr-rt-punct-in-quote rt) rt)))
		 (lambda (x) (s-replace "ʼ" "’" x))))

(defun cpr-rt--attr-values (r attr)
  "Return the list of ATTR values in raw rich-text content R.
The values are ordered depth-first."
  (if (listp r)
      (let ((val (alist-get attr (car r)))
	    (body-vals (--mapcat (cpr-rt--attr-values it attr) (cdr r))))
	(if val (cons val body-vals)
	  body-vals))
    nil))

(defun cpr-rt-rendered-name-ids (r)
  "Return the list of name ids in raw content R."
  (cpr-rt--attr-values r 'name-id))

(defun cpr-rt-endered-vars (r)
  "Return the list of rendered vars in raw content R."
  (cpr-rt--attr-values r 'rendered-var))

(defun cpr-rt-rendered-date-vars (r)
  "Return the list of date vars in raw content R."
  (--select (memq it cpr--date-vars) (cpr-rt-endered-vars r)))

(defun cpr-rt-rendered-name-vars (r)
  "Return the list of name vars in raw content R."
  (--select (memq it cpr--name-vars) (cpr-rt-endered-vars r)))

(provide 'cpr-rt)

;;; cpr-rt.el ends here
