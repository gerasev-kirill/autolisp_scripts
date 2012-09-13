;
;###################################################################################
;	AutoLisp
;	Модуль для программ
;	Отстраивание УП по кадрам
; 	Функции работы с фасками, дугами, линиями.
;###################################################################################
;	Герасев Кирилл
;	gerasev.kirill@gmail.com
;	08/05/2012
;###################################################################################
;	

;		чтение из файла
; 		возвращение в виде списка

(defun read_from_txt( / _l  _program  _f _res)
		(setq _f (open (findfile "УП.txt") "r"))
		(setq _program nil)
		(if (/= (findfile "УП.txt") nil)
		  (progn
				(setq _f (open (findfile "УП.txt") "r"))
				(setq _l (read-line _f) )

				(while (/= _l nil )
					(if  (/= (strfirst _l) "(")
						(if (/= (strlen _l) 0) 
								(setq _program (append _program (list _l)))
						)
					)
					(setq _l (read-line _f) )
				)
			(close _f)
		  )
		  )
		(setq _res _program)
)

;	вывод отладочной инфы
(defun debug (text debug_info)
	(princ text)
	(princ " : ")
	(princ debug_info)
	(princ "\n")
)

;	последний символ из строки
(defun  strlast (string / _res 	_len)
	(setq _len (strlen string))
	(setq _res (substr string _len))
)

; 	первый символ из строки
(defun strfirst (  string / _res)
	(setq _res (substr string 1 1))
)

;	взятие символа из строки по индексу
(defun strindex (string index / _res)
	(setq _res (substr string index 1))
)

;	вывод проверочных кадров
(defun print_verify (kadr info)
	(princ "№ ")
	(princ kadr)
	(princ ": ")
	(princ info)
	(princ "\n")
)


; 	вытягивание из слова только числа (для G кодов)
(defun get_number(word / _last 	_i 	_mod 	_res _first)
	(setq _last (-  (strlen word) 1))
	(setq _first 0)
	(setq _i 1)
	(while (< _i (+ 1 (strlen word) ))
		(setq _mod (strindex word _i))
		(if (or (= _mod "*") (= _mod "@") (= _mod "~") )
			(if (< _i 2) 
						(setq _first (+ _first 1))
						(setq _last (- _last 1))
			)
		)
		(setq _i (+ _i 1))
	)
	(if (= _first 0)
		(setq _first 2)
		(setq _first (+ 2 _first))
	)
	(setq _res (substr word _first _last))
	(if (= _res "...")
		(setq _res 100)
		(setq _res (atof _res))
	)
;	(debug )
	(if (> (abs _res) 100)
			(setq _res (/ _res 1000))
	)
	;(if (= "G" (strindex word 2))
	
	;)
		(setq _res _res)
)


; есть ли символ в строке
(defun char_in_str (text  char / _tmp	 _res)
	(setq _res nil)
	(setq _i 1)
	(while (< _i (+ 1 (strlen text)))
		(setq _tmp (strindex text _i))
		(if (= _tmp char)
				(setq _res T)
		)
		(setq _i (+ _i 1))
	)
	(setq _res _res)
)

; отрисовка точки
(defun draw_point(koords)
	(command "_color" 250)
	(command "_point" (list (/ (car koords	) 2) (last koords)))
	(setq GK_LAST_POINT (list (/ (car koords	) 2) (last koords)))
)

; отрисовка линии	
; ось, координаты(последней точки в линии), тип(сплошная или штриховая)
(defun draw_line(axis koords type / _tmp)
	(if (= type "axeled")
		(command "_color" 132)
		(command "_color" 250)
	)
	(if (/= axis "NONE")
		(progn 
			(if (= axis "X+")
				(setq _tmp (list (+ (/ koords 2) (car GK_LAST_POINT)) (last GK_LAST_POINT )))
			)
			(if (= axis "Z+")
				(setq _tmp (list (car GK_LAST_POINT) (+ koords (last GK_LAST_POINT ))))
			)
			(if (= axis "X")
				(setq _tmp (list  (/ koords 2) (last GK_LAST_POINT )))
			)
			(if (= axis "Z")
				(setq _tmp (list (car GK_LAST_POINT) koords ))
			)
			(if (= axis "DUAL")
					(setq _tmp (list (+ (/ (car koords) 2) (car GK_LAST_POINT)) (+ (last koords) (last GK_LAST_POINT ))))
			)
			(if (= axis "45")
				(setq _tmp koords)
			)
		)
		(setq _tmp (list (/ (car koords) 2) (last koords)))
	)
;	(setq _tmp (list (/ (car _tmp) 2) (last _tmp)))
;	(debug "last = " GK_LAST_POINT)
	
	(command "ОТРЕЗОК"  GK_LAST_POINT _tmp "")
	(setq GK_LAST_POINT _tmp) 
)


; 		отрисовка прямоугольника 
(defun draw_rect(axis koords type / _tmp)
	(if (= type "axeled")
		(command "_color" 132)
		(command "_color" 250)
	)
	(if (/= axis "NONE")
		(progn 
			(if (= axis "X")
				(setq _tmp (list (+ koords (car GK_LAST_POINT)) (last GK_LAST_POINT )))
				(setq _tmp (list ((car GK_LAST_POINT)) (+ koords (last GK_LAST_POINT ))))
			)
			(if (= axis "DUAL")
					(setq _tmp (list (+ (car koords) (car GK_LAST_POINT)) (+ (last koords) (last GK_LAST_POINT ))))
			)
		)
		(setq _tmp (list (/ (car koords) 2) (nth 1 koords)))
	)
	(command "ПРЯМОУГОЛЬНИК"  GK_LAST_POINT _tmp)
)


; отрисовка дуги
;FIXME: переписать
(defun draw_arc(axis center last_point direction  /   koords	_tmp)
	(command "_color" 250)
	(setq koords last_point)
	(if (/= axis "NONE")
		(progn 
			(if (= axis "X")
				(setq last_point (list (+ koords (car GK_LAST_POINT)) (last GK_LAST_POINT )))
				(setq last_point (list ((car GK_LAST_POINT)) (+ koords (last GK_LAST_POINT ))))
			)
			(if (= axis "DUAL")
					(setq last_point (list (+ (car koords) (car GK_LAST_POINT)) (+ (last koords) (last GK_LAST_POINT ))))
			)
		)
	)
	
	(if (= direction "G3")
		(progn
			(setq _tmp GK_LAST_POINT  )
			(setq GK_LAST_POINT  last_point )
			(setq last_point  _tmp)
		)
	)
	(command "ДУГА" "Ц" center  GK_LAST_POINT last_point)
	(setq GK_LAST_POINT last_point)
)
