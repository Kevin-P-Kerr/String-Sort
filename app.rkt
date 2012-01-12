#lang web-server/insta
(require "model.rkt" web-server/formlets)

(define (start request)
 (render-main-page (initalize-master! (build-path (current-directory))) request))

(define (render-main-page master request)
  (local [(define (response-generator make-url)
            (response/xexpr 
             `(html (head (title "Sort Your Numbers!"))
                    (body (p "Enter a list of space seperated numbers to see them sorted!")
                           (li ,@(render-greatest master))
                           (li ,@(render-least master))
                          (form ([action 
                                  ,(make-url string-sort-handler)])
                                  ,@(formlet-display string-sort-formlet)
                                  (input ([type "submit"])))))))
          
          (define (string-sort-handler request)
            (define-values (stringnumbers)
            (formlet-process string-sort-formlet request))
            (cleanse-table master)
            (master-numberstring-insert! master stringnumbers)
            (render-main-page master request))
         
         (define (render-greatest master)
           (greatest-sort-string master))
         
         (define (render-least master)
           (least-sort-string master))]
    
    (send/suspend/dispatch response-generator)))

(define string-sort-formlet
  (formlet 
   (#%# ,{input-string . =>  . stringnumbers})
   (values stringnumbers)))
            
               

