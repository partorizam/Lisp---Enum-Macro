;;;; Author - Marc Khristian Partoriza
;;;; Date   - March 16th, 2015
;;;; Class  - ICS 313
;;;;
;;;; This program imlements an enum facility as a Common Lisp macro, defenum,
;;;; to add functionality similar to Java's enum types. The program does not
;;;; include any iteration constructs.
;;;;
;;;; 42 lines of code.



;;; defenum - A macro that takes in a list of type fields (types) and the enum arguments (enums)
;;;           corresponding to the types fields. Creates an association list for each variable
;;;           and a reverse lookup function for defenum.
(defmacro defenum (types &rest enums)
  `(progn
     ,@(createconstants (createnames types enums))
     ,@(createfunctions types enums)))



;;; createnames - A function thats takes in a list of type fields (types and the enum arguments (enums)
;;;               corresponding to the type fields and returns a list of sublists containing names needed
;;;               to create defconstants in function 'createconstants'
(defun createnames (types enums)
  (setf finallist '())
  (setf typename (first types))
  (mapcar (lambda (enum)
             (setf enumname (first enum))
             (setf list (mapcar #'list (rest types) (rest enum)))
             (setf newlist (mapcar (lambda (name)
                                      (setf typefield (first name))
                                      (setf name (rest name))
                                      (setf name (cons enumname name))
                                      (setf name (cons typefield name))
                                      (setf name (cons typename name))
                                      (setf finallist (cons name finallist)))
                             list)))
    enums)
  (reverse finallist))



;;; createconstants - A function thats takes in a list of names (by createnames) and creates defconstants
;;;                   for each name in a certain format (EX: DAY->NUMBER->SUNDAY)         
(defun createconstants (listofnames)
  (mapcar (lambda (name)
             (setf value     (first (rest (rest (rest name)))))
             `(defconstant ,(intern (concatenate 'string
                                    (symbol-name (first name)) "->"
                                    (symbol-name (first (rest name))) "->"
                                      (symbol-name (first (rest (rest name)))))) ,value))
    listofnames))



;;; createfunctions - A function thats takes in a list of type fields (types and the enum arguments (enums)
;;;               corresponding to the type fields and creates a reverse lookup function for each type field
(defun createfunctions (types enums)
  (mapcar (lambda (typefield)
             (setf list '())
             (setf typepos (position typefield types :test #'equal))
             (mapcar (lambda (enum)
                       (setf typevalue (nth typepos enum))
                       (setf pair (first (pairlis (list typevalue) (list (first enum)))))
                       (setf list (cons pair list))) enums)

             (setf typename->typefield (intern (concatenate 'string
                                                 (symbol-name (first types)) "->"
                                                 (symbol-name typefield))))
             
             `(let ((,typename->typefield ,(reverse list)))
                (defun ,typename->typefield (,typefield) (cdr (assoc ,typefield ,typename->typefield)))))
    (rest types)))
