#!/usr/local/bin/gosh
(use net.twitter)
(use sxml.sxpath)

(define *cred* (make <twitter-cred>
					 :consumer-key "consumer-key"
					 :consumer-secret "consumer-secret"
					 :access-token "access-token"
					 :access-token-secret "access-token-secret"))

(define (user-get user-id)
  (let ((sxml (twitter-user-show/sxml *cred* :screen-name user-id)))
	`(,(append ((sxpath "//user/statuses_count/text()")sxml) ;;ツイート数
			   ((sxpath "//user/friends_count/text()")sxml) ;;フォロー数
			   ((sxpath "//user/followers_count/text()")sxml) ;;フォロワー数
			   ((sxpath "//user/favourites_count/text()")sxml)) ;;ふぁぼ数
	   ,(map(lambda(x)(x->number x))(twitter-followers/ids *cred* :screen-name user-id))))) ;;フォロワー

(define (user-show id)
  (let* ((sxml (twitter-user-show/sxml *cred* :id id)))
	((sxpath "//user/screen_name/text()")sxml)))

(define (lst? lst ls)
  (let loop ((lst lst)(result '()))
	(cond
	  ((null? lst)
	   result)
	  ((memv (car lst) ls)
	   (loop (cdr lst) result))
	  (else (loop (cdr lst) (cons (car lst) result))))))

(define (main args)
  (display "user> ")
  (flush)
  (let* ((user-id (x->string (read)))
		 (file-name (string-append "./."user-id))
		 (result (user-get user-id))
		 (solve (if (file-exists? file-name)
				  (let ((lst (with-input-from-file file-name (pa$ read))))
					`(,(map (lambda(x y)(- (x->number x) (x->number y)))(car result)(car lst)) 
							,(let ((hoge (lst? (map (lambda(x)(x->number x)) (sort (cadr lst)))
											   (map (lambda(x)(x->number x)) (sort (cadr result))))))
							  (if (null? hoge)
								'()
								(map(lambda(x)(user-show x))hoge)))))
				  '((0 0 0 0) ()))))
	(print "ツイート数: "(list-ref (car result) 0))
	(print "  変化: "(list-ref (car solve) 0))
	(print "フォロー数: "(list-ref (car result) 1))
	(print "  変化: "(list-ref (car solve) 1))
	(print "フォロワー数: "(list-ref (car result) 2))
	(print "  変化: "(list-ref (car solve) 2))
	(print "fav数: "(list-ref (car result) 3))
	(print "  変化: "(list-ref (car solve) 3))
	(print "--------詳細--------")
	(if (null? (cadr solve))
			 (print "特になし")
			 (print (string-join (map(lambda(x)(string-append "@"(car x)))(cadr solve)) " ")"にリムられた"))
	(with-output-to-file file-name (pa$ print result) :if-exists :overwrite :if-does-not-exist :create)))
