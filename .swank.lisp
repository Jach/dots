(in-package :swank)

; from swank-fancy-inspector.lisp:
; changed grouping to inheritance (groups by class).
; If order is unsorted, then it shows
; the order defined in the source.

(defvar *inspector-slots-default-order* :alphabetically
  "Accepted values: :alphabetically and :unsorted")

(defvar *inspector-slots-default-grouping* :inheritance
  "Accepted values: :inheritance and :all")

; fix for swank-asdf.lisp's depends-on, mine is called sys-depends-on
(defmethod xref-doit ((type (eql :sys-depends-on)) thing)
  "asdf's depends-on first seems to be direct dependencies,
   and the rest is the list of packages"
  (loop for dependency in (rest (first (asdf:component-depends-on 'asdf:operation thing)))
        for asd-file = (asdf:system-definition-pathname dependency)
        when asd-file
        collect (list dependency
                      (swank/backend:make-location
                        `(:file ,(namestring asd-file))
                        `(:position 1)
                        `(:snippet ,(format nil "(defsystem :~A" dependency)
                          :align t)))))

