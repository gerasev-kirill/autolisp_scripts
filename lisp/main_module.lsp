;###################################################################################
;	Модуль для программ
;	Перебор точек, сохранение в таблицы, сохранение в файлы и т.д.
;	Для токарных станков с повернутой системой координат X/2-Z.
;	Для обычных координат с НЕ повернутыми координатами X-Z
;###################################################################################
;	Переменная для опеределения расположения координатных осей:
;	acad_asix = {	
;			"x/2-z" 	- для токарных станков		
;			"x-z"		-  для фрезерных и остальных
;			}
;###################################################################################
;	Герасев Кирилл
;	25/04/2012
;###################################################################################
;		 ФУНКЦИИ МОДУЛЯ:
;
;		 find_real_coord ( list_of_coord  acad_axis ) => list_of_coord
;		 revers_order_of_coord( list_of_coord ) => list_of_coord
;		 mk_difference ( list_of_coord ) => list_of_coord 
;		 bubltoarc ( pt1 pt2 bubl ) => ( cnt r (angle cnt pt1) (angle cnt pt2) )
;		 debug (text debug_info)
; 		 draw_point_and_text ( list_of_coord ) 
;  		 print_to_table( list_of_coord title acad_axis )
;		 coord_to_string( n_t ) => ( string )
;

;###################################################################################
; общее для всех программ
;
(defun min1(f)
 (cond 
  ((null f) nil)
  (1 (min2 (car f) (cdr f)))
 )
)

(defun min2(c f)
 (cond 
  ((null f) c)
  ((> c (car f)) (min2 (car f) (cdr f)))
  (1 (min2 c (cdr f)))
 )
)

(defun remov(c f)
 (cond 
  ((null f) nil)
  ((= c (car f)) (cdr f))
  (1 (cons (car f) (remov c (cdr f))))
 )
)


(defun sort_int(f)
 (cond
   ((null f) nil)
   (1 (cons (min1 f) (sort_int (remov (min1 f) f))))
  )
)



(defun real_X_Y(coord acad_axis / _realX _realY _res) ; возвращение списка с реальными координатами
	(if (= acad_axis "x/2-z")
		(progn	; для токарных станков
			(setq _realX (nth 1 coord))
			(setq _realX (* -1 coord))
			(setq _realY (car coord))
			(list (list _realX _realY))
		)
		(	;для фрезерных и остальных
			(setq _res coord)
		)
	)
)

(defun find_real_coord (list_of_coord  acad_axis /  ; найти реальные координаты для чертежа
												_res  _tmp  _realX  _realY) 
	(if (= acad_axis "x/2-z");если токарный станок, то
		(progn
			(setq	_res NIL)
			( foreach _tmp list_of_coord
				(setq _realX (nth 1 _tmp))
				(setq _realX (* -1 _realX))
				(setq _realY (car _tmp))
				(if (> (LENGTH _tmp) 2)
					(progn
						(setq _res ( append _res (list (list _realX _realY (nth 2 _tmp)))))
					)
					(setq _res ( append _res (list (list _realX _realY))))
				)
			)
		)
		; иначе вернуть тоже что и было
		(setq _res list_of_coord)
	)
)

(defun revers_order_of_coord(list_of_coord / _res _n) 	; обратная сортировка координат точек
	(setq _res NIL)
	(setq _n  (LENGTH list_of_coord))
	(while (/= _n 0)
		(setq _n (1- _n ))
		(setq _res (append _res (list (nth _n list_of_coord))))
	)
)

(defun mk_difference (list_of_coord / _dif  _last_coord  _X  _Y _tmp_coord) ; определение  относительных координат
	(setq	_dif NIL)
	; считается относительно 1 точки в списке
	(setq _last_coord (car list_of_coord))
	(foreach _tmp list_of_coord 
		(setq _X ( - (car _tmp) (car _last_coord)))
		(setq _Y ( - (nth 1 _tmp) (nth 1 _last_coord) ))
		(setq _last_coord _tmp)
		(if (> (LENGTH _tmp) 2)
			(progn 
				; для дуг
				(setq _dif ( append _dif (list (list _X _Y (nth 2 _tmp)))))
			)
			(setq _dif ( append _dif (list (list _X _Y))))
		)
	)
)

(defun from_object_to_coord(object /  _n _lst_point _res _tmp ) ; определение координат из объектов типа линий, точек, полилиний
	(setq _lst_point NIL)
	(setq _n 0)
	(while ( <  (+ _n 3) (LENGTH object)); полилиния
		(setq _tmp (nth _n object))
		(if (= ( car _tmp) 10 )  
			(progn
				(if (/= (cdr (nth (+ _n 3) object )) 0 )
					(setq _lst_point (append _lst_point (list ( list (round (nth 1 _tmp) 3)  (round (nth 2 _tmp)  3) (cdr (nth (+ _n 3) object ))	))))
					(setq _lst_point (append _lst_point (list ( list (round (nth 1 _tmp)  3) (round (nth 2 _tmp) 3)))))
				)
			)
		)
		(setq _n (1+ _n))
	)
	(if (< (LENGTH object) 20); линия
		(progn
			(foreach _tmp object
				(if (= (car _tmp ) 10)
					(setq _lst_point (append _lst_point (list ( list (round (nth 1 _tmp) 3) (round (nth 2 _tmp) 3)))))
				)
				(if (= (car _tmp ) 11)
						(setq _lst_point (append _lst_point (list ( list (round (nth 1 _tmp) 3) (round (nth 2 _tmp) 3)))))
				)
			)
		)
	)
	(setq _res _lst_point)
)

(defun bubltoarc (pt1 pt2 bubl / dst cnt r); переводит представление дуги из polyline в arc.
  ;начальная _point, конечная, значение 42 группы
	(setq 	dst (distance pt1 pt2)
		r	(+ (* (/ dst 2.0) bubl) (/ (- (* (/ dst 2.0) (/ dst 2.0))
			(* (/ (* dst bubl) 2.0) (/ (* dst bubl) 2.0))) (* dst bubl)))
		cnt	(polar
			(polar pt1 (angle pt1 pt2) (/ dst 2.0))
			(- (angle pt1 pt2) (/ pi 2.0))
			(- (* (/ dst 2.0) bubl) r))
	);end of setq
	(list cnt r (angle cnt pt1) (angle cnt pt2))
  ;точки центра дуги, радиус, начального угла дуги, конечного угла дуги
)

(defun get_current_layer( / _res)
	(setq _res (getvar "CLAYER" ))
	(if (= (substr _res 1 2) "T-")
		(setq _res (list _res  (substr _res 3 2)))
	)
)

(defun is_T(layers / _res _T_lay) ; является ли слой слоем с расчетными точками движения инструмента
	(setq _T_lay nil)
	(foreach lay layers
		(if (= (substr lay 1 2) "T-")
			(setq _T_lay (append _T_lay (list lay)))
		)
	)
	(setq _res _T_lay)
)

(defun copy_to_layer (layer_name weigth /  _material _weigth _res ); сопирование из слоя текущего в слой с именем layer_name
	(setq _material (ssget "_w" (list  -200  -200) (list  200 200)))
	(command "_copytolayer" _material "" layer_name (list 0 0) (list 0 weigth))
	(princ)
)

(defun get_date ( / d yr mo day) ; получение даты
     (setq d (rtos (getvar "CDATE") 2 6)
	 
          yr (substr d 3 2)
		  
          mo (substr d 5 2)
		  
         day (substr d 7 2)
		 
     )
     (strcat day "." mo "." yr)
)

(defun get_all_info( / _operation _modification _name   _full_path _res _res1)
	; получение информации из иерархии папок. стуктура расположения проекта должна быть:
	;root/Имя_детали/Номер_детали_и_номер_изделия/Операция
	;FIXME: 	подумать на счет изменения структуры расположения файлов
	(setq _full_path (GETVAR "dwgprefix"))
	(setq _operation (get_last_folder _full_path))
	(setq _modification (get_last_folder (last _operation)))
	(setq _name (get_last_folder (last _modification)))
	(setq _res (list
				(car _name)
				(car _modification)
				(car _operation)
	))
)

(defun get_last_folder( folder_name /  _i  _res   _tmp  )
	; функция для определения родительской папки и имени последней папки в строке
	; только для WINDOWS
	; FIXME: 	предусмотреть для LINUX
	; возвращается: 1-имя папки; 2-путь к родительской папке
	(setq _i (- (strlen folder_name) 1))
	(while (> _i 0)
		(setq _tmp (substr folder_name _i 1))
		(if (= _tmp "\\")
			(progn
				(setq _res _i)
				(setq _i  -1)
			)				
		)
		(setq _i (- _i 1))
	)
	(setq  _res1 (+ _res 1))
	(setq _res (list (substr folder_name  _res1  ( - (strlen folder_name) _res1) )))
	(setq _res (append _res (list (substr folder_name 1  (- _res1 1)))))
)

(defun get_dwg_name( / _folder_name _i  _tmp  _res)
	(setq _folder_name (GETVAR "dwgprefix"))
	(setq _res (get_last_folder _folder_name))
)

(defun get_layers_names( / _ln  _layers _res)
	
	(vl-load-com)
	(vlax-for layer (vla-get-layers (vla-get-activedocument (vlax-get-acad-object)))
    (vl-catch-all-apply
      (function
        (lambda ()
          (setq _ln (vla-get-name layer))
        )
      )
    )
	; в списке возвращается все слои кроме  Defpoints и 0
	(if (not (or (equal _ln "Defpoints") (equal  _ln "0")))
		(setq _layers (append _layers (list _ln)))
	)
  )
  (setq _res _layers)
)


;###################################################################################
; вспомогательные функции печати и отрисовки

(defun debug (text debug_info) ; вывод отладочной информации
	(princ "\n")
	(princ text)
	(princ " ")
	(princ debug_info)
	(princ "\n")
)

(defun draw_all(_lst_point index  acad_axis /  _rev_order _res _sys_var)
	; отрисовка точек . прямой и обратный порядок
	
	(setq _sys_var (mapcar 'getvar '("osmode" "cmdecho")))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	;;;;;;;
	
	(setq  _lst_point (find_real_coord _lst_point acad_axis))
	(setq GK_LIST_OF_COORD _lst_point)
	(if (> (nth 1 (car _lst_point))	(nth 1 (last _lst_point)))
		(setq GK_ORDER "right")
		(setq GK_ORDER "left")  
	)
	(draw_point_and_text 	_lst_point 	 acad_axis   index)
	(setq _rev_order 0)
	(setq answ (getstring "\nСделать обратный порядок точек? <Д/Н>"))
	(if ( OR ( = answ "Д" ) ( = answ "д" ) )
		(progn
			(command "_undo"  1)
			(setq _lst_point (revers_order_of_coord _lst_point))
			(draw_point_and_text _lst_point		acad_axis   index)
			(setq _rev_order 1)
			(setq GK_LIST_OF_COORD _lst_point)
		)
	)
	(setq GK_ABSOLUTE 1)
	
	;;;;;;;
	(mapcar 'setvar '("osmode" "cmdecho") sys_var)
	(setq _res (list _rev_order _lst_point))
)

(defun draw_point_and_text (list_of_coord  acad_axis  index / _n  _undo_tmp _angle_of_text)
	(vla-startundomark
		(setq _undo_tmp (vla-get-activedocument (vlax-get-acad-object)))
    ) 
	; на bricscad не работает=(
	
	(setq _n index)
	(command "_color" GK_COLOR)
	(if (= acad_axis "x/2-z")
		(setq _angle_of_text 90)
		(setq _angle_of_text 0)
	)
	(foreach _tmp list_of_coord
		(command "_point" (list (car _tmp) (nth 1 _tmp)))
		; FIXME:		расставлять _text на расстоянии в соответствии с стилем.
		(command "_text" (polar (list (car _tmp) (nth 1 _tmp)) (/ pi 4) 2) _angle_of_text (rtos _n 2))
		(setq _n (1+ _n))
	)
	(command "_color" "_BYLAYER")
	
	(vla-endundomark _undo_tmp) 
)

(defun draw_circle_and_text (list_of_coord 	radius	index  acad_axis  side / _n  _undo_tmp _angle_of_text _tmp_fake)
	;  функция для простановки окружностей радиусом radius вместо простых точек. 
	;  также проставляются точки на осях окружности
	;  точки не будут проставляться, если четверть не будет в диапазоне 1...4
	;  напр.
	;	draw_circle_and_text (list 0 0) 8 0 "axis" 4	<= 	"4" - означает, что точки будут проставляться в 4 четверти
	;	FiXME:		"axis" - цвет отрисовки примитивов
	;									--
	;							--	       --
	;					 --						 Х
	;							--			--
	;									Х
	;
	(vla-startundomark
		(setq _undo_tmp (vla-get-activedocument (vlax-get-acad-object)))
    ) 
	
	(command "_color" GK_COLOR )
	(setq _n index)
	(if (= acad_axis "x/2-z")
		(setq _angle_of_text 90)
		(setq _angle_of_text 0)
	)
	(foreach _tmp list_of_coord
		(command "CIRCLE" (list (car _tmp) (nth 1 _tmp)) radius)
		(command "_text" (polar (list (car _tmp) (nth 1 _tmp)) (/ pi 4) 2) _angle_of_text (rtos _n 2))
		(setq _n (1+ _n))
		(cond 
			((= side 1) ; первая четверть
				(command "_point"  (list (car _tmp) (+ (nth 1 _tmp) radius)) )
				(command "_point" (list (- (car _tmp) radius) (nth 1 _tmp)))
			)
			((= side 2) ; вторая четверть
				(command "_point" (list (car _tmp) (- (nth 1 _tmp) radius)))
				(command "_point" (list (- (car _tmp) radius) (nth 1 _tmp)))
			)
			((= side 3) ;  3 четверть
				(command "_point" (list (car _tmp) (- (nth 1 _tmp) radius)))
				(command "_point" (list (+ (car _tmp) radius) (nth 1 _tmp)))
			)
			((= side 4) ; 4 четверть
				(command "_point" (list (car _tmp) (+ (nth 1 _tmp) radius)))
				(command "_point" (list (+ (car _tmp) radius) (nth 1 _tmp)))
			)		
		)
	)
	(command "_color" "_BYLAYER")
	
	(vla-endundomark _undo_tmp)
)

(defun get_font_size( / _text_style 	_text_size_s		_res)
	; возвращение размера шрифта из названия текущего стиля
	; напр: 	"по ГОСТ 3" => 3
	; FIXME:		переделать так, чтоб не надо было указывать явно размер шрифта в имени стиля
	(setq _text_style (getvar "TEXTSTYLE"))
	(setq _text_size_s (substr _text_style
													(strlen _text_style)
													))
	(setq _res (atoi _text_size_s ))
)

(defun round (var to / _res)
		; функция округления
		; почему ее сделал не помню%)
		(setq _res (rtos var 2 to ))
		(setq _res (atof _res))
) 

(defun print_to_table(list_of_coord   title    acad_axis index / _model_space  _pt  _n  _X  _Y		_font_size)	; печать в таблицу
	(vl-load-com)
	(setq _model_space (vla-get-Modelspace(vla-get-ActiveDocument(vlax-get-acad-object))))  
	; на bricscad не работает  (vla-get-ActiveDocument(vlax-get-acad-object))
	(setq _pt (getpoint "\nТочка вставки таблицы "))
	
	(if (or (= acad_axis "x/2-z" )  (= acad_axis "x/2-z-spesial" ))
		(progn
			; грязный мега-хак, так делается только для координат X/2-Z
			(setq _X (nth 1 _pt))
			(setq _Y (car _pt))
			(setq _Y (* -1 _Y))
			(setq _pt (list _X _Y 0) )
		)
	)
	(setq _font_size (get_font_size))
	(setq cnt (LENGTH list_of_coord))
	(setq myTable (vla-AddTable 
                    _model_space
                    (vlax-3d-point _pt)
                    (+ 2 cnt)
                    3
                    0.7
                    (+ (* _font_size 2) 5)
					))
	(vla-setText mytable 0 0 title)
	(vla-setText mytable 1 0 "№")
	(vla-setText mytable 1 1 "Х")
	(if (or (= acad_axis "x/2-z" )  (= acad_axis "x/2-z-spesial" ))
		(vla-setText mytable 1 2 "Z")
		(vla-setText mytable 1 2 "Y")
	)
	
	(setq _n  0)
	
	(foreach _tmp_coord list_of_coord
		(setq _X (car _tmp_coord))
		(if (= acad_axis "x/2-z" )
			(setq _X (*  2 (round _X 3 )))
		)
		(setq _Y (nth 1 _tmp_coord))
		(setq _X (rtos _X 2 3 ))
		(setq _Y (rtos _Y 2 3 ))
		
		(vla-setText mytable (+ 2 _n) 0 index )
		(vla-setText mytable (+ 2 _n) 1 _X)
		(vla-setText mytable (+ 2 _n) 2 _Y)
		(setq _n (1+ _n ))
		(setq index (+ index 1))
	)
)

; EXCEL
; прежде чем использовать эксель, необходимо его инициализировать
; FIXME: подлежит удалению	init_excel, convert_num_to_cell, print_to_excel.
; переписать все с импортом в обычный файл
(defun init_excel( / _file   _file_name_template		_file_prefix)
	(if (not GK_XL) ; глобальная переменная с именем файла
		(progn
			(setq _file_name_template "c:/template.xls")
			(setq _file_prefix (getvar "DWGPREFIX")) 
			(setq _res_xls 0)
			(setq _n 1)
			; копирование файла шаблона из папки программы в папку с чертежем
			(setq _file (strcat _file_prefix  GK_CP_NAME))
			(vl-file-copy  _file_name_template _file)
			; работа с activeX - создание файла в екселе, запуск сессии.
			(vl-load-com)
			(setq GK_XL (vlax-get-or-create-object "Excel.Application"))
			(vlax-put-property GK_XL "Visible" :vlax-true)
			(vlax-get-property GK_XL "ActiveSheet")
			(vlax-invoke-method (vlax-get-property GK_XL "Workbooks") "Open" _file)	
		)
	)
	(princ GK_XL)
)
	
(defun convert_num_to_cell(number / _res); из числа в строку с именем ячейки
	; 
	(setq _res (+ 4 number))
	(cond 
		(( and (> number -1 ) (< number 20))
			(setq _res (strcat "B" (itoa
					(+ 4 number)	
			)))
		)
		(( and (> number 19 ) (< number 40))
			(setq _res (strcat "E" (itoa
					(+ 4 (- number 20))
			)))
		)
		(( and (> number 39 ) (< number 60))
			(setq _res (strcat "G" (itoa
					(+ 4 (- number 40))
			)))
		)
		(( and (> number 59 ) (< number 80))
			(setq _res (strcat "I" (itoa
					(+ 4 (- number 60))
			)))
		)
		(( and (> number 79 ) (< number 100))
			(setq _res (strcat "K" (itoa
					(+ 4 (- number 80))
			)))
		)
		(( and (> number 99 ) (< number 120))
			(setq _res (strcat "M" (itoa
					(+ 4 (- number 100))
			)))
		)
		(( and (> number 119 ) (< number 139))
			(setq _res (strcat "O" (itoa
					(+ 4 (- number 120))
			)))
		)
		(( and (> number 140 ) (< number 150))
			(setq _res (strcat "Q" (itoa
					(+ 4 (- number 140))
			)))
		)
	)
)

(defun print_to_clipboard(list_of_codes number)
	(setq _string "")
	(foreach _tmp2 list_of_codes
		(setq _string (strcat _string _tmp2 "\n"))
	)
	(setClipText _string)
)

(defun  print_to_excel(list_of_codes number / _tmp2  _i   _tmp_cell _res _dcl_id)
	(init_excel)
	(if (= number 0)
		(progn
				(setq _dcl_file "c:/gk_autocad/gk_excel.dcl")
				(setq _dcl_id (load_dialog _dcl_file))
				(if (not (new_dialog "number_cp" _dcl_id))
					(exit)
				)
				(action_tile "accept"	"(setq number (get_tile \"eb_numner_cp\"))(done_dialog)")
				(action_tile "cancel"	"(done_dialog)")
				(start_dialog)
				(unload_dialog _dcl_id)
				(setq _i (atoi number))
		)
		(setq _i number)
	)
	(foreach _tmp2 list_of_codes
			(setq _tmp_cell (vlax-get-property GK_XL "Range"		
												(convert_num_to_cell _i)))
			(vlax-put-property _tmp_cell "Value2" _tmp2)
			(setq _i (+ 1 _i))
	)
	(princ)
)


(defun setClipText(str / html result)
	(if (= 'STR (type str))
	  (progn
	  (setq html   (vlax-create-object "htmlfile")
			result (vlax-invoke (vlax-get (vlax-get html 'ParentWindow) 'ClipBoardData) 'setData "Text" str)
	  )
	  (vlax-release-object html)
	   str
	   )
	)
	(princ)
)


;###################################################################################
; функции работы с G -кодами	(общие)
; преобразование в строки
(defun C:get_table_context(/ _n _program_text obj _i _kord  _k  _inkremZ)
	(setq _program_text nil)
	(vl-load-com)
	(while (setq obj (car (entsel)))
		(progn
			(setq obj (vlax-ename->vla-object obj))
			(if (= (vla-get-ObjectName obj) "AcDbTable")
				(progn
					(setq _i 2)
					(setq _kord nil)
					(setq _inkremZ (getint "Введите р-р Z <-2>"))
					(while (< _i (vla-get-rows obj))
						(setq _x (atof (vla-GetText obj _i  1)))
						(setq _z (atof (vla-GetText obj _i 2)))
						(setq _kord (append _kord  (list (list _x _z) )))
						(setq _i (+ _i 1))
					)
					(setq _program_text (append _program_text (list (list (strcat "Z-" (ctsn33 (abs _inkremZ)))))))
					(setq _n 0)
					(while (< _n (LENGTH _kord ))
						(setq _k (nth _n _kord))
						(if (/= 0 (nth 0 _k))
						(progn
							(setq _x (nth 0 _k))
							(if (> _x 0)
								(setq _x (strcat "+" (ctsn33 _x)))
								(setq _x (strcat "-" (ctsn33 (abs _x))))
							)
							(setq _x (strcat "X" _x))
							)
							(setq _x 0)
						)
						(if (/= 0 (nth 1 _k))
						(progn
							(setq _y (nth 1 _k))
							(if (> _y 0)
								(setq _y (strcat "+" (ctsn33 _y)))
								(setq _y (strcat "-" (ctsn33 (abs _y))))
							)
							(setq _y (strcat "Y" _y))
							)
							(setq _y 0)
						)
						(setq _n (+ 1 _n))
						(if (and (/= _x 0) (/= _y 0))
							(setq _program_text (append _program_text (list(list _x _y))))
							(progn
								(if (/= _x 0)
									(setq _program_text (append _program_text (list (list _x))))
								)
								(if (/= 0 _y)
									(setq _program_text (append _program_text (list (list _y))))
								)
							)
						)
						
						)
					)
					)
  (princ _program_text)
  	(setq _string "")
	(foreach _tmp2 _program_text
		(setq _tmp4 "NNNN")
		(foreach _tmp3 _tmp2
			(setq _tmp4 (strcat _tmp4 _tmp3))
		)
		(setq _string (strcat _string _tmp4 "\n"))
	)
	(setClipText _string)
	)
)
)


; лучше эти функции не трогать(!), они держатся на МАГИИ

(defun get_part(str _from / _result) ; см. описание ниже
	(setq str (substr str _from))
	(if (= (strlen str) 0)
		(setq _result "000")
		(progn
			(if (= (strlen str) 1)
				(setq _result (strcat str "00"))
				(progn
					(if (= (strlen str) 2)
						(setq _result (strcat str "0"))
						(if  (= (strlen str) 3)
							(setq _result str)
							(if (> (strlen str) 3)
									(setq _result (substr str 1 3))
							)
						)
					)
					)
				)
			)
		)
	)
  
 (defun ctsn33(n_t / _res _part)
	(setq _res (coord_to_string n_t))
	(setq _res (strcat "0" _res))
	(princ _res)
	(princ "\n")
	(setq _res (substr _res 1 (- (strlen _res) 1)))
	(setq __i (strlen _res))
	(while (< __i 6)
		(setq _res (strcat "0" _res))
		(setq __i (+ __i 1))
	)
	(setq _res _res)
 )
  
(defun coord_to_string( n_t / _result _res part) ; преобразование числа в строку
	; формат: 	89.09756 => "89097"
	(if (equal n_t 0.0 0.00001)
			(setq n_t 0)
	)
	(if (= n_t 0)
		(setq _result "0")
		(progn
			(if (/= (fix n_t) 0)
				(if (> n_t 0)
					(progn
						(setq from (strlen (rtos (fix n_t))))
						(setq from (+ from 2))
						(setq part (get_part (rtos n_t 2 4) from))
						(setq _result (itoa (fix n_t)))
						(if (equal ( - n_t  (fix n_t)) 1.0 0.001)
							(progn
							(setq _result (rtos n_t))
							)

							)
						(setq _result (strcat _result part))
					)
					(progn
						(setq from (strlen (rtos (fix n_t))))
						(setq from (+ from 2))
						(setq part (get_part (rtos n_t 2 4) from))
						(setq _result (itoa (fix n_t)))
						(setq _result (strcat _result part))
					)
				)
			)
			(if (= (fix n_t) 0 )
				(if (> n_t 0)
					(progn
						(setq part (get_part (rtos n_t 2 4) 3))
						(setq _result  part)
					)
					(progn
						(setq part (get_part (rtos n_t 2 4) 4))
						(setq _result (strcat "-" part))
					)
				)
			)
		)
	)
	(setq _res _result)
 )
