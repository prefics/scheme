;;; curses.scm -- CURSES interface for Scheme

;; Interface

      ;; curses.scm
      curses-initscreen curses-endwin curses-write! curses-move!
      $cursor/invisible $cursor/visible $cursor/very-visible
      curses-set-cursor! call-with-cursor 
      curses-refresh! curses-getch curses-clear!
      $attribute/normal $attribute/standout $attribute/underline
      $attribute/reverse $attribute/bold
      curses-set-attr!
      call-with-curses
      $key/min
      $key/break $key/down $key/up $key/left $key/right $key/home
      $key/backspace $key/f0 $key/f1 $key/f2 $key/f3 $key/f4 $key/f5          
      $key/f6 $key/f7 $key/f8 $key/f9 $key/f10 $key/f11 $key/f12         
      $key/dl $key/il $key/dc $key/ic $key/eic $key/clear $key/eos         
      $key/eol $key/sf $key/sr $key/npage $key/ppage $key/stab        
      $key/ctab $key/catab $key/enter $key/sreset $key/reset       
      $key/print $key/ll $key/a1 $key/a3 $key/b2 $key/c1 $key/c3          
      $key/btab $key/beg $key/cancel $key/close $key/command $key/copy        
      $key/create $key/end $key/exit $key/find $key/help $key/mark $key/message
      $key/move $key/next $key/open $key/options $key/previous $key/redo
      $key/reference $key/refresh $key/replace $key/restart $key/resume      
      $key/save $key/sbeg $key/scancel $key/scommand $key/scopy       
      $key/screate $key/sdc $key/sdl $key/select $key/send $key/seol        
      $key/sexit $key/sfind $key/shelp $key/shome $key/sic $key/sleft       
      $key/smessage $key/smove $key/snext $key/soptions $key/sprevious   
      $key/sprint $key/sredo $key/sreplace $key/sright $key/srsume      
      $key/ssave $key/ssuspend $key/sundo $key/suspend $key/undo $key/mouse
      $key/max         
      $mouse/button1-released $mouse/button1-pressed $mouse/button1-clicked
      $mouse/button1-double-clicked $mouse/button1-tripple-clicked
      $mouse/button2-released $mouse/button2-pressed $mouse/button2-clicked
      $mouse/button2-double-clicked $mouse/button2-tripple-clicked
      $mouse/button3-released $mouse/button3-pressed $mouse/button3-clicked
      $mouse/button3-double-clicked $mouse/button3-tripple-clicked
      (with-curses :syntax)

(define $return/err (- 2 3))

(define (curses-initscreen)
  (curses-initscreen))

(define (curses-endwin)
  (curses-endwin))

(define (curses-write! char/string)
  (curses-write char/string))

(define (curses-move! y x)
  (curses-move y x))

(define $cursor/invisible 0)
(define $cursor/visible 1)
(define $cursor/very-visible 2)

(define (curses-set-cursor! val)
  (let ((old-cursor (curses-setcursor val)))
    (if (= old-cursor $return/error)
	#f
	old-cursor)))

(define (call-with-cursor cursor thunk)
  (let ((old #f))
    (dynamic-wind 
	(lambda () (set! old (curses-setcursor! cursor)))
	thunk
	(lambda () (curses-setcursor! old)))))

(define (curses-refresh!)
  (curses-refresh))

(define (curses-getch timeout)
  (curses-getch timeout))

(define (curses-clear!)
  (curses-clear))

(define *attributes* (curses-attributes))
(define $attribute/normal    0)
(define $attribute/standout  1)
(define $attribute/underline 2)
(define $attribute/reverse   3)
(define $attribute/bold      4)

(define *current-attribute* $attribute/normal)
(define-syntax with-attribute
  (syntax-rules ()
    ((with-attribute ?attr . ?body)
     (let ((before *current-attribute*))
       (dynamic-wind 
           (lambda () (curses-set-attr! ?attr))
           (lambda () . ?body)
           (lambda () (curses-set-attr! before)))))))

(define (curses-set-attr! attr)
  (curses-attrset (vector-ref *attributes* attr)))

;;; Currently not supported

(define $color/black   0)
(define $color/red     1)
(define $color/green   2)
(define $color/yellow  3)
(define $color/blue    4)
(define $color/magenta 5)
(define $color/cyan    6)
(define $color/white   7)

(define (curses-set-color! fore back)
  (curses-set-color fore back))

(define (call-with-curses thunk)
  (let ((dimension #f))
    (dynamic-wind
	(lambda () (set! dimension (curses-initscreen)))
	(lambda () (thunk (car dimension) (cdr dimension)))
	(lambda () (curses-endwin)))))

(define-syntax with-curses
  (syntax-rules ()
    ((with-curses ?thunk)
     (call-with-curses ?thunk))))

;;; KEY values

(define $key/min         257)            ;; Minimum curses key 
(define $key/break       257)            ;; break key (unreliable) 
(define $key/down        258)            ;; Sent by terminal down arrow key 
(define $key/up          259)            ;; Sent by terminal up arrow key 
(define $key/left        260)            ;; Sent by terminal left arrow key 
(define $key/right       261)            ;; Sent by terminal right arrow key 
(define $key/home        262)            ;; Sent by home key. 
(define $key/backspace   263)            ;; Sent by backspace key 
(define $key/f0          264)            ;; function key f0. 
(define $key/f1          265)
(define $key/f2          266)
(define $key/f3          267)
(define $key/f4          268)
(define $key/f5          269)
(define $key/f6          270)
(define $key/f7          271)
(define $key/f8          272)
(define $key/f9          273)
(define $key/f10         274)
(define $key/f11         275)
(define $key/f12         276)

(define $key/dl          330)            ;; Sent by delete line key. 
(define $key/il          331)            ;; Sent by insert line. 
(define $key/dc          332)            ;; Sent by delete character key. 
(define $key/ic          333)            ;; Sent by ins char/enter mode key. 
(define $key/eic         334)            ;; Sent by rmir or smir in ins mode. 
(define $key/clear       335)            ;; Sent by clear screen or erase key. 
(define $key/eos         336)            ;; Sent by clear-to-end-of-screen. 
(define $key/eol         337)            ;; Sent by clear-to-end-of-line key. 
(define $key/sf          338)            ;; Sent by scroll-forward/down key 
(define $key/sr          339)            ;; Sent by scroll-backward/up key 
(define $key/npage       340)            ;; Sent by next-page key 
(define $key/ppage       341)            ;; Sent by previous-page key 
(define $key/stab        342)            ;; Sent by set-tab key 
(define $key/ctab        343)            ;; Sent by clear-tab key 
(define $key/catab       344)            ;; Sent by clear-all-tabs key. 
(define $key/enter       345)            ;; Enter/send (unreliable) 
(define $key/sreset      346)            ;; soft (partial) reset (unreliable) 
(define $key/reset       347)            ;; reset or hard reset (unreliable) 
(define $key/print       348)            ;; print or copy 
(define $key/ll          349)            ;; Sent by home-down key 

(define $key/a1          350)            ;; Upper left of keypad 
(define $key/a3          352)            ;; Upper right of keypad 
(define $key/b2          353)            ;; Center of keypad 
(define $key/c1          354)            ;; Lower left of keypad 
(define $key/c3          355)            ;; Lower right of keypad 
(define $key/btab        356)            ;; Back tab key 
(define $key/beg         357)            ;; beg(inning) key 
(define $key/cancel      358)            ;; cancel key 
(define $key/close       359)            ;; close key 
(define $key/command     360)            ;; cmd (command) key 
(define $key/copy        361)            ;; copy key 
(define $key/create      362)            ;; create key 
(define $key/end         363)            ;; end key 
(define $key/exit        364)            ;; exit key 
(define $key/find        365)            ;; find key 
(define $key/help        366)            ;; help key 
(define $key/mark        367)            ;; mark key 
(define $key/message     368)            ;; message key 
(define $key/move        369)            ;; move key 
(define $key/next        370)            ;; next object key 
(define $key/open        371)            ;; open key 
(define $key/options     372)            ;; options key 
(define $key/previous    373)            ;; previous object key 
(define $key/redo        374)            ;; redo key 
(define $key/reference   375)            ;; ref(erence) key 
(define $key/refresh     376)            ;; refresh key 
(define $key/replace     377)            ;; replace key 
(define $key/restart     378)            ;; restart key 
(define $key/resume      379)            ;; resume key 
(define $key/save        380)            ;; save key 
(define $key/sbeg        381)            ;; shifted beginning key 
(define $key/scancel     382)            ;; shifted cancel key 
(define $key/scommand    383)            ;; shifted command key 
(define $key/scopy       384)            ;; shifted copy key 
(define $key/screate     385)            ;; shifted create key 
(define $key/sdc         386)            ;; shifted delete char key 
(define $key/sdl         387)            ;; shifted delete line key 
(define $key/select      388)            ;; select key 
(define $key/send        389)            ;; shifted end key 
(define $key/seol        390)            ;; shifted clear line key 
(define $key/sexit       391)            ;; shifted exit key 
(define $key/sfind       392)            ;; shifted find key 
(define $key/shelp       393)            ;; shifted help key 
(define $key/shome       394)            ;; shifted home key 
(define $key/sic         395)            ;; shifted input key 
(define $key/sleft       396)            ;; shifted left arrow key 
(define $key/smessage    397)            ;; shifted message key 
(define $key/smove       398)            ;; shifted move key 
(define $key/snext       399)            ;; shifted next key 
(define $key/soptions    400)            ;; shifted options key 
(define $key/sprevious   401)            ;; shifted prev key 
(define $key/sprint      402)            ;; shifted print key 
(define $key/sredo       403)            ;; shifted redo key 
(define $key/sreplace    404)            ;; shifted replace key 
(define $key/sright      405)            ;; shifted right arrow 
(define $key/srsume      406)            ;; shifted resume key 
(define $key/ssave       407)            ;; shifted save key 
(define $key/ssuspend    408)            ;; shifted suspend key 
(define $key/sundo       409)            ;; shifted undo key 
(define $key/suspend     410)            ;; suspend key 
(define $key/undo        411)            ;; undo key 
(define $key/mouse       412)            ;; Mouse event has occured 
(define $key/max         511)            ;; Maximum curses key 

;;; Mouse Support

(define $mouse/button1-released        1)
(define $mouse/button1-pressed         2)
(define $mouse/button1-clicked         4)
(define $mouse/button1-double-clicked  8)
(define $mouse/button1-tripple-clicked 16)
(define $mouse/button2-released        64)
(define $mouse/button2-pressed         128)
(define $mouse/button2-clicked         256)
(define $mouse/button2-double-clicked  512)
(define $mouse/button2-tripple-clicked 1024)
(define $mouse/button3-released        2048)
(define $mouse/button3-pressed         4096)
(define $mouse/button3-clicked         8192)
(define $mouse/button3-double-clicked  16384)
(define $mouse/button3-tripple-clicked 32768)


; (define (test)
;   (call-with-curses 
;    (lambda (w h)
;      (display w) (display h)
;      (curses-move! 0 0)
;      (let loop ((ch (curses-getch (- 2 3))))
;        (cond ((vector? ch) (display ch) (loop (curses-getch (- 2 3))))
; 	     ((= ch (char->integer #\space)) 'ok)
; 	     (else (curses-write! (number->string ch))
; 		   (loop (curses-getch (- 2 3)))))))))

