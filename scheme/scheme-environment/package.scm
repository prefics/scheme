;;; package.scm -- library definition file

(define-structure scheme-environment
  (export ;; lib.scm
   <library-definition> library-definition?
   library-definition-name
   library-definition-file-name set-library-definition-file-name!
   library-definition-export set-library-definition-export!
   library-definition-import set-library-definition-import!
   library-definition-doc    set-library-definition-doc!
   library-definition-files  set-library-definition-files!

   make-empty-library-definition
   parse-library-definition
   read-library-definition-from-file
   library-definitions
   library-definition-names

   ->library-definition
   add-library-definition
   remove-library-definition
   
   lookup-library-definition
   load-library-definition

   current-library current-library-name with-current-library

   ;; fasl.scm

   load-library
   load-file
   load-fasl-file

   ensure-library-loaded
   locate-library

   ;; compilation.scm

   compile-library

   ;; script.scm
   source-filename

   ;; system.scm
   
   <system-definiton> system-definition?
   system-definition-name
   system-definition-version
   system-definition-author
   system-definition-author-email
   system-definition-file-name
   system-definition-parents
   system-definition-description
   system-definition-components

   make-empty-system-definition

   read-system-definition-from-file
   
   <component>
   <library-component> make-library-component library-component?
   library-component-file-name

   <files-component> make-files-component file-component?
   files-component-file-names

   system-definition-paths
   add-system-path! remove-system-path!
   system-definitions
   lookup-system-definition
   add-system-definition
   remove-system-definition
   locate-system-in-directory

   add-system remove-system
   
   compile-system

   load-system-from-definition
   load-system force-load-system

   install-system

   package-system

   upload-system

   compile-component
   load-component
   test-component
   install-component
   package-component
   upload-component

   )
  (open scheme-runtime scheme-posix scheme-compiler)
  (doc "Scheme Environment Support")
  (files lib.scm
	 io.scm
	 compilation.scm
	 script.scm
	 system.scm))