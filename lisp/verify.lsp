;
;###################################################################################
;	AutoCAD 2007 + AutoLISP + Visual LISP
;	Программа проверки кода УП для НЦ-31
;###################################################################################
;	Герасев Кирилл
;	07/05/2012
;###################################################################################
;	
(load "verify_module")
(load "main_module")
(setq GK_B 0)

;		нахождение координаты для угла в 45 град.
(defun get_45_koords(word / _direct2 _direct3 _X _Z _W _res)
	(setq _res nil)
	(if (> (strlen word) 5)
	(progn
		(setq _direct2 (substr word (- (strlen word) 2) 3))
		(setq _direct3 (substr word (- (strlen word) 3) 4))
		(setq _X (car GK_LAST_POINT))
		(setq _Z (nth 1 GK_LAST_POINT))
		(setq _W (get_number word))
		(cond
			((or (= _direct2 "+45") ( = _direct3  "+45*"))
				(if (= (strfirst word) "X")
					(progn 
						(setq _W (/ _W 2))
						(if (char_in_str word "@")
								(setq _W  (+ _W (car GK_LAST_POINT)))
						)
						(setq _Z ( + _Z (abs (- _X _W))))
						(setq _res (list _W _Z))
						)
					(progn ; Z
						(if (char_in_str word "@")
								(setq _W  (+ _W (nth 1  GK_LAST_POINT)))
						)
						(setq _X ( + _X   ( abs (- _Z _W)) ))
						(setq _res (list _X _W))
					)
				)
			)
			((or (= _direct2 "-45") ( = _direct3  "-45*"))
				(if (= (strfirst word) "X")
					(progn 
						(setq _W (/ _W 2))
						(if (char_in_str word "@")
								(setq _W  (+ _W (car GK_LAST_POINT)))
						)
						(setq _Z ( - _Z  (abs (- _X _W)) ))
						(setq _res (list _W _Z))
					)
					(progn ; Z
						(if (char_in_str word "@")
								(setq _W  (+ _W (nth 1 GK_LAST_POINT)))
						)
						(setq _X ( - _X   ( abs (- _Z _W)) ))
						(setq _res (list _X _W))
					)
				)
			)
		)
	)
	)
	(setq _res _res)
)


;		ядро интерпретатора
(defun interpret (kadrs number / _axis _tmp _axeled _m45)
	(setq _addres (strfirst (car kadrs)))
	(if (= (substr (car kadrs) 2 1) "G")
		(setq _addres (substr (car kadrs) 2 1))
	)
	(cond
		;((= _addres "M")
		;	(make_M_code kadrs number)

		;)
		((= _addres "G")
			(make_G_code kadrs number)
		)
		((= _addres "T")
			(setq GK_B 0)
			(if (char_in_str (car kadrs) "b")
			(progn
			;	(setq _b (getreal (strcat "Ширина резца для инструмента " (strindex (car kadrs) 2) " = ")))
				(setq _bb (substr  (car kadrs)  (+ 2 (indx_char_in_str  (car kadrs)  "b")) 3))
				(setq GK_B (atof _bb))
				)
			)
;			(print_verify number (strcat "Установка инструмента " (strindex (car kadrs) 2)))
			(setq _layers (get_layers_names))
			(command "_LAYER" "_S" "0" "")
			(foreach  _lay 	_layers
				(command "_LAYER" "_OFF" _lay "")
			)
			
			(command "_LAYER" "_M" (strcat "verify T" (strindex (car kadrs) 2)) "")
			(setq _ss (ssget "_X" (list (cons 8 (strcat "verify T" (strindex (car kadrs) 2)))) ))
			(command "_erase" _ss "")
			(draw_point (list 70 70))
		)

		((or (= _addres "X") ( = _addres "Z"))
			(if (> (LENGTH kadrs) 1)
				(setq _tmp (list
									(get_number (car kadrs))
									(get_number (last kadrs))
				))
				(setq _tmp (get_number (car kadrs)))
			)
			(setq _m45 (get_45_koords (car kadrs)))
			(if (=  _m45 nil)
				(progn
				(if (> (LENGTH kadrs) 1)
						(progn
							(if  (char_in_str  (car kadrs) "@")  
								(setq _axis "DUAL")
								(progn
									(if (= (strfirst (nth 1 kadrs))  "F")
										(progn
											(setq _axis (substr (car kadrs) 1 1 ))
											(setq _tmp (car _tmp))
										)
										(setq _axis "NONE")
									)
								)
							)
							(if (char_in_str (car kadrs) "~")
								(setq _axeled "axeled")
							)
						)
						(progn
							(if (char_in_str (car kadrs) "~")
									(setq _axeled "axeled")
							)
							(if  (char_in_str  (car kadrs) "@")  
								(setq _axis (strcat (substr (car kadrs )1 1 ) "+"))
								(setq _axis (substr (car kadrs) 1 1 ))
							)
						)
				)
				)
				(progn
					(setq _tmp _m45)
					(setq _axeled nil)
					(setq _axis "45")		
				)
			)
				(setq _word (car kadrs))
				;(if (substr _word (- (strlen _word) 3) 3))
				(draw_line _axis _tmp _axeled)
		)
	)

)


(defun verify(kadrs / _word _modificator _i  _kortej)
	(setq _kortej nil)
	(setq _i 0)
	(foreach _word kadrs
		(if  (/= (strlen _word) 0) 
			(progn
				(setq _modificator (strlast _word))
				(if (= _modificator "*")
					(setq _kortej (append _kortej  (list	_word)))
				)
				(if (and (/= _modificator "*") (> (LENGTH _kortej ) 0))
					(progn
						(setq _kortej (append _kortej  (list	_word)))
						(interpret 	_kortej 	(- _i  (LENGTH _kortej)))
						(setq _kortej nil)
					)
				(if (and (/= _modificator "*") (= (LENGTH _kortej ) 0))
					(progn
						(interpret	 (list _word	) 	_i)
					)
				)
				
				)
				(setq _i (+ _i 1))
			)
		)
	)

)

;	 	проверка текста программы из текстового файла.
(defun C:GK-NC31-VERIFY-FROM-TXT( /  _word 	_i  _kortej)
	(setq sys_var (mapcar 'getvar '("osmode" "cmdecho")))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)

	(setq program (read_from_txt))
;	(setq _kortej nil)
;	(setq _i 0)
	(draw_point (list 70 70))
;	(foreach _word program
;		(if  (/= (strlen _word) 0) 
;			(progn
;				(setq _modificator (strlast _word))
;				(if (= _modificator "*")
;					(setq _kortej (append _kortej  (list	_word)))
;				)
;				(if (and (/= _modificator "*") (> (LENGTH _kortej ) 0))
;					(progn
;						(setq _kortej (append _kortej  (list	_word)))
;						(interpret 	_kortej 	(- _i  (LENGTH _kortej)))
;						(setq _kortej nil)
;					)
;				(if (and (/= _modificator "*") (= (LENGTH _kortej ) 0))
;					(progn
;						(interpret	 (list _word	) 	_i)
;					)
;				)
;				
;				)
;				(setq _i (+ _i 1))
;			)
;		)
;	)
	(verify program)
	(setq _layers (get_layers_names))
	(command "_LAYER" "_S" "0" "")
	(foreach  _lay 	_layers
			(command "_LAYER" "_OFF" _lay "")
	)
	(command "_LAYER" "_M" "verify T1"  "")
	(mapcar 'setvar '("osmode" "cmdecho") sys_var)
)


;	чтение М-кодов
;	кортеж слов, номер кадра с которого произошло чтение
(defun make_M_code(words number / _m_kode )
	(setq _fword (car words))
	(setq _code (get_number _fword))
			(if (= _code 3)
					(princ "!!!!!!")
				)
	(cond
			((= _code 0)
				(print_verify number "Останов")
			)
	
			((= _code 1)
				(print_verify number "Останов с подтверждением")
			)
	
			((= _code 3)
				(print_verify number "Вращение шпинделя по часовой стрелке")
			)
	
			((= _code 4)
				(print_verify number "Вращение шпинделя против часовой стрелки")
			)
	
			((= _code 8)
				(print_verify number "Включение охлаждения")
			)
	
			((= _code 9)
				(print_verify number "Отключение охлаждения")
			)
	
			((or (< _code 43) (> _code 40))
				(print_verify number "Установка диапазона")
			)
			
			((> _code 0)
				(print_verify number  (strcat "Неопознанная команда - М" _code))
			)
	)
)

;		чтение G-кодов
;	кортеж слов, номер кадра с которого произошло чтение
(defun make_G_code(kadrs  number / _g_code _f_word _code _tmp  _p1 _p2)
	(setq _fword (car kadrs))
	(setq _code (get_number _fword))
	
	(if (> (LENGTH kadrs) 3)
			(if (= (strfirst (nth 2 kadrs)) "Z")
				(setq _tmp (list
									(/ (get_number (nth 1 kadrs)) 2)
									(get_number (nth 2 kadrs))
				))
			)
			(setq _tmp nil)
	)
	(cond
		((= _code 75)

			(draw_rect "NONE" _tmp "col")
		
		)
		((= _code 25)
			(setq _f1 (get_number (nth 1 kadrs)))
			(setq _f2 (* 1000 (- _f1 (fix _f1))))
			(setq _f1 (fix _f1))
			(setq _f2 (fix (+ 0.2 _f2)))
			(setq _mylist nil)
			(princ (strcat "Повтор кадров " (itoa _f1 ) " по " (itoa _f2 ) "  : "))
			(while (/= _f1 (+ 1 _f2))
				(setq _mylist (append _mylist (list
																						(nth (+ 1 _f1) program)
																						)))
				(setq _f1 (+ 1 _f1))
			)
			
			(princ _mylist)
			(princ "\n")
			(verify _mylist)
		)
		((or (= _code 78) (= _code 77))
			(if (/= _tmp nil)
				(progn
					(if (> (LENGTH kadrs) 1)
						(progn
							(if  (char_in_str  (nth 1 kadrs) "@")  
								(setq _axis "DUAL")
								(setq _axis "NONE")
							)
							(if (char_in_str (nth 1 kadrs) "~")
								(setq _axeled "axeled")
							)
						)
						(progn
							(if (char_in_str (nth 1 kadrs) "~")
									(setq _axeled "axeled")
							)
							(if  (char_in_str  (nth 1 kadrs) "@")  
								(setq _axis (strcat (substr (nth 1 kadrs )1 1 ) "+"))
								(setq _axis (substr (nth 1 kadrs) 1 1 ))
							)
						)
					)
					(draw_rect _axis _tmp _axeled)
				)
			)
		)
		((= _code 3)
			(setq _p (list
									(get_number (nth 3 kadrs))
									(get_number (nth 4 kadrs))
			))
			
			(if  (char_in_str  (nth 1 kadrs) "@")  
								(setq _axis "X")
								(setq _axis "NONE")
			)
			(draw_arc _axis _p _tmp _fword)
		)
		((= _code 2)
			(setq _p (list
									(get_number (nth 3 kadrs))
									(get_number (nth 4 kadrs))
			))
			
			(if  (char_in_str  (nth 1 kadrs) "@")  
								(setq _axis "DUAL")
								(setq _axis "NONE")
			)
			(draw_arc _axis _p _tmp _fword)
		)
		((= _code  13)
			(setq _tmp (list
									(/ (get_number (nth 1 kadrs)) 2)
									(get_number (nth 2 kadrs))
			))
			(if  (char_in_str  (nth 1 kadrs) "@")  
								(setq _axis "DUAL")
								(setq _axis "NONE")
			)
			
			(setq koords _tmp)
			(if (= _axis "X")
				(setq _p2 (list (+ koords (car GK_LAST_POINT)) (last GK_LAST_POINT )))
			)
			(if (= _axis "Z")
				(setq _p2 (list ((car GK_LAST_POINT)) (+ koords (last GK_LAST_POINT ))))
			)
			(if (= _axis "DUAL")
					(setq _p2 (list (+ (car koords) (car GK_LAST_POINT)) (+ (last koords) (last GK_LAST_POINT ))))
					(setq _p2 _tmp)
			)
			(debug "p2 =" _p2)
			(setq _p1 GK_LAST_POINT)
			(setq _p (- (car _p1) (car _p2)))
			
			(if (> _p 0)
				(if (> (nth 1 _p1) (nth 1 _p2))
					(setq _c (list 0 (abs _p)))
					(setq _c (list  (abs _p) 0))
				)
				(if (> (nth 1 _p1) (nth 1 _p2))
					(setq _c (list (abs _p) 0))
					(setq _c (list  0 (abs _p)))
				)
				
			)
			(draw_arc "NONE" _c _tmp "G3*")
			(setq GK_LAST_POINT (list (car _p2)   (nth 1 _p2)))
			(debug "Hellp " GK_LAST_POINT)
		)
		
		((= _code  12)
			(setq _tmp (list
									(/ (get_number (nth 1 kadrs)) 2)
									(get_number (nth 2 kadrs))
			))
			(if  (char_in_str  (nth 1 kadrs) "@")  
								(setq _axis "DUAL")
								(setq _axis "NONE")
			)
			
			(setq koords _tmp)
			(if (= _axis "X")
				(setq _p2 (list (+ koords (car GK_LAST_POINT)) (last GK_LAST_POINT )))
			)
			(if (= _axis "Z")
				(setq _p2 (list ((car GK_LAST_POINT)) (+ koords (last GK_LAST_POINT ))))
			)
			(if (= _axis "DUAL")
					(setq _p2 (list (+ (car koords) (car GK_LAST_POINT)) (+ (last koords) (last GK_LAST_POINT ))))
					(setq _p2 _tmp)
			)
			
			
			(setq _p1 GK_LAST_POINT)
			(setq _p (- (car _p1) (car _p2)))
			
			(if (> _p 0)
				(if (> (nth 1 _p1) (nth 1 _p2))
					(setq _c (list  (abs _p) 0))
					(setq _c (list 0 (abs _p)))
				)
				(if (> (nth 1 _p1) (nth 1 _p2))
					(setq _c (list  0 (abs _p)))
					(setq _c (list (abs _p) 0))
				)
				
			)
			(draw_arc "NONE" _c _p2 "G2*")
		)
	)
)
