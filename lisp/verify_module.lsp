;###################################################################################
;	Модуль для программ
;	Отстраивание УП по кадрам
; 	Функции разбора фасок, дуг, линий
;###################################################################################
;	Герасев Кирилл
;	08/05/2012
;###################################################################################
;	

;		чтение из файла
; 		возвращение в виде списка
(load "main_module")
(setq GK_COLOR_F 6)

(defun read_from_txt( / _l  _program  _f _res)

		(setq _f (get_dwg_name))
		(setq _f (open (strcat (nth 1 _f) (nth 0 _f)  "\\УП.txt") "r"))
		(setq _program nil)
		(setq _l (read-line _f) )

		(while (/= _l nil )
			(if  (/= (strfirst _l) "(")
				(setq _program (append _program (list _l)))
			)
			(setq _l (read-line _f) )
		)
		(close _f)
		(setq _res _program)
)

;	вывод отладочной инфы
(defun debug (text debug_info)
	(princ text)
	(princ debug_info)
	(princ "\n")
)

;	последний символ из слова
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


; 	вытягивание из слова только цыфры
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
	(if (> (abs _res) 100)
			(setq _res (/ _res 1000))
	)
	
	(setq _res _res)
)

(defun indx_char_in_str (text  char / _tmp	 _res)
	(setq _res nil)
	(setq _i 1)
	(while (< _i (+ 1 (strlen text)))
		(setq _tmp (strindex text _i))
		(if (= _tmp char)
				(setq _res _i)
		)
		(setq _i (+ _i 1))
	)
	(setq _res _res)
)

;		есть ли символ в строке
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

;		отрисовка точки
(defun draw_point(koords)
	(command "_color" GK_COLOR_F)
	(command "_point" (list (/ (car koords	) 2) (last koords)))
	(setq GK_LAST_POINT (list (/ (car koords	) 2) (last koords)))
)

;		отрисовка линии	
;		ось, координаты(последней точки в линии), тип(сплошная или штриховая)
(defun draw_line(axis koords type / _tmp)
	(if (= type "axeled")
		(command "_color" 132)
		(command "_color" GK_COLOR_F)
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
	(command "_line"  GK_LAST_POINT _tmp "")
	(setq GK_LAST_POINT _tmp) 
)


(defun draw_rect(axis koords type / _tmp)
	(if (= type "axeled")
		(command "_color" 132)
		(command "_color" GK_COLOR_F)
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
		(setq _tmp koords)
	)
	(command "_rectang"  GK_LAST_POINT _tmp)
	(if (/= GK_B 0)
		(progn
			(command "_color" 2)
			(command "_rectang"  GK_LAST_POINT (list (car _tmp) (+ (nth 1 GK_LAST_POINT) GK_B)))
		)
	)
	;(setq GK_LAST_POINT (list (car GK_LAST_POINT) (nth 1 _tmp)))
)


;		отрисовка дуги
(defun draw_arc(axis p last_point direction  /   koords	_p1 _p2	_tmp)
	(command "_color" 5)
	(setq koords last_point)
	(if (/= axis "NONE")
		(progn 
			(if (= axis "X")
				(setq last_point (list (+ koords (car GK_LAST_POINT)) (last GK_LAST_POINT )))
			)
			(if (= axis "Z")
				(setq last_point (list ((car GK_LAST_POINT)) (+ koords (last GK_LAST_POINT ))))
			)
			(if (= axis "DUAL")
					(setq last_point (list (+ (car koords) (car GK_LAST_POINT)) (+ (last koords) (last GK_LAST_POINT ))))
			)
		)
	)
	
	(if (= direction "G3*")
		(progn
			(setq _p1 GK_LAST_POINT )
			(setq _p2  last_point)
			(if (> (car _p1) (car _p2))
				(if (> (nth 1 _p1) (nth 1 _p2))
						(setq _c (list 
													(+ (car _p1) (car p))
													(- (nth 1 _p1) (nth 1 p))
						)); 1 
						(setq _c (list 
													(- (car _p1) (car p))
													(- (nth 1 _p1) (nth 1 p))
						)) ; 4
						
				)
				(progn
					(if (> (nth 1 _p1) (nth 1 _p2))
							(setq _c (list 
														(+ (car _p1) (car p))
														(+ (nth 1 _p1) (nth 1 p))
							)); 2
							(setq _c (list 
														(- (car _p1) (car p))
														(+ (nth 1 _p1) (nth 1 p))
							)) ; 3
					)
				)
			)
			(command "_arc" "_C" _c  _p1 _p2 )
			(setq GK_LAST_POINT  last_point )
		)
		; if G2
		(progn
			(setq _p1 GK_LAST_POINT )
			(setq _p2  last_point)
			
			(if (> (car _p1) (car _p2))
				(if (> (nth 1 _p1) (nth 1 _p2))
						(setq _c (list 
													(- (car _p1) (car p))
													(+ (nth 1 _p1) (nth 1 p))
						)); 3
						(setq _c (list 
													(+ (car _p1) (car p))
													(+ (nth 1 _p1) (nth 1 p))
						)) ; 2
						
				)
				(progn
					(if (> (nth 1 _p1) (nth 1 _p2))
							(setq _c (list 
														(- (car _p1) (car p))
														(- (nth 1 _p1) (nth 1 p))
							)); 4
							(setq _c (list 
														(+ (car _p1) (car p))
														(- (nth 1 _p1) (nth 1 p))
							)) ; 1
					)
				)
			)
			(command "_arc" "_C" _c  _p2 _p1)
			(setq GK_LAST_POINT  last_point )
		
		)
	)
	(command "_point" _c)
	(command "_color" GK_COLOR_F)
)

(defun draw_trapezoid(last_point p2 direction)
	(princ "Отрисовка трапеции пока невозможна")
)
