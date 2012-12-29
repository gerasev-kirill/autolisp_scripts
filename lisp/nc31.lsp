;
;###################################################################################
;	AutoCAD 2007 + AutoLISP + Visual LISP
;	Программа преобразования полилиний в таблицы с координатами в "изломах"
;		* вывод в acad таблицах,
;		* вывод в xls-файлы,
;		* вывод в G-кодах.
;	Для токарных станков с повернутой системой координат X/2-Z.
;###################################################################################
;	Герасев Кирилл
;	14/04/2012
;###################################################################################
; 	Для работы _необходимо_ иметь специальные слои "деталь" и "заготовка"
; 	Таблицы и точки _НЕ_ПРОСТАВЛЯЮТСЯ_ на слоях которые не начинаются с "T-"
; 	

;###################################################################################
; загрузка модуля с вспомогательными функциями

(load "main_module")
;FIXME: 		 необходимо дописать окошко для редактирования параметров.
(setq GK_USE_45 1 )
(setq GK_45_AXIS "X") 
(setq GK_USE_F 1 )
(setq GK_F 0 )
(setq GK_USE_~ 1)
(setq GK_NOT_USE_P 0 )
(setq GK_CP_NAME  "УП.xls")

;###################################################################################
; специальные функции разбора дуг на функции G03/G02
; представления линий под углом в 45 град в фаски
;

(defun make_45 (point angle / _X  _Z  _res)
	(setq _X (coord_to_string (*  2  (car point) )))
	(setq _Z (coord_to_string (last point )))
	
	(cond 
		((equal angle 45 0.1)
			(progn
				(if (= GK_45_AXIS "X")
						(setq _res (strcat "X" _X "+45*"))
						(setq _res (strcat "Z" _Z "+45*"))
				)
			)
		)

		((equal angle 135 0.1)
			(progn
				(if (= GK_45_AXIS "X")
						(setq _res (strcat "X" _X "+45*"))
						(setq _res (strcat "Z" _Z "-45*"))
				)
			)
		)

		((equal  angle 225 0.1)
			(progn
				(if (= GK_45_AXIS "X")
						(setq _res (strcat "X" _X "-45*"))
						(setq _res (strcat "Z" _Z "-45*"))
				)
			)
		)

		((equal angle 315 0.1)
			(progn
				(if (= GK_45_AXIS "X")
						(setq _res (strcat "X" _X "-45*"))
						(setq _res (strcat "Z" _Z "+45*"))
				)
			)
		)
		
	)
		
	(setq _res (list _res))
	(if (= GK_USE_F 1)
		(setq _res ( append _res (list (strcat "F" (itoa GK_F)))))	
	)
)

(defun make_g2_3_g12_13 ( point_with_tan	point  	revers  _coords_for_print / _X, _Z, _p1, _p2, _G, _b, tang, _coords, _GP1, _GP2, _r)
	(setq _p1 (list (car point_with_tan) (nth 1 point_with_tan)))
	(setq _p2 (list (car point) (nth 1 point)))
	(setq tang (last point_with_tan))
	(setq _coords (bubltoarc _p1 _p2  tang))
	
	(setq _angle (angle _p1 _p2))
	
	(if (= revers 0)
		(setq _b (nth 2 _coords))
		(setq _b (nth 3 _coords))
	)
	
	( COND 
		( (and (> _angle 0) (< _angle (/ pi 2) ))
							;обработка дуги в "I"  и "III" четверти обратного порядка
							(if (< tang 0)
									(setq _G "G2*") ;"I" 
							(progn ;"III"
									(setq _G "G3*")
									(setq _b (- _b pi))
								)
							)
		)
		
		( (and (> _angle (/ pi 2)) (< _angle pi))
							;обработка дуги в "II" и "IV" четверти обратного порядка
							(if (< tang 0)
								(progn ; "II"
									(setq _G "G2*")
									(setq _b (- pi _b))
								)
								(progn ; "IV"
									(setq _G "G3*")
									(setq _b (- (* 2 pi) _b))
								)
							)
		)

;		переработано!!
		
		( (and (> _angle pi) (< _angle (* (/ 3 2 ) pi)))
							;обработка дуги в "I" и "III" четверти прямого порядка
							(if (< tang 0)
								(progn ; "III"
									(setq _G "G2*")
									(setq _b (- (* 2 pi) _b ))
								)
								(progn ; "I"
									(setq _G "G3*")
									(setq _b (- _b (/ pi 2)))
								)
							)
		)
		
		(  (and (> _angle (* (/ 3 2 ) pi)) (< _angle (* 2 pi)))
							;обработка дуги в "II" и "IV" четверти прямого порядка
							(if (< tang 0)
								(progn ; "IV"
									(setq _G "G2*")
									(setq _b (- (/  pi  2) _b))
								)
								(progn ; "II"
									(setq _G "G3*")
									(setq _b (- (/ (* 3 pi) 2) _b))
								)
							)
		)
	)
	
	(setq _r (abs (nth 1 _coords)))	
	(setq _GP1 (abs (* (sin _b) _r)))
	(setq _GP2 (abs (* (cos _b) _r)))
	
	(setq _p2 (car _coords_for_print))
	(setq _p1 (nth 1 _coords_for_print))
	
	(setq _X (coord_to_string (*  2  (car _p2 ) ) ))
	(setq _Z (coord_to_string  (nth 1 _p2 ) ))
	
	(if (= revers 1)
		(progn
			(if (= _G "G3*")
				(setq _G "G2*")
				(setq _G "G3*")
			)
			(setq _X (coord_to_string (*  2 (car _p1 ) )))
			(setq _Z (coord_to_string  (last _p1 ) ))
		)
	)
	
	; отработка на наличие дуги с углом в 90 град.
	(setq _GP1 (round _GP1 3))
	(setq _GP2 (round _GP2 3))
	(debug "_GP1 = " _GP1)
	(debug "_GP2 = " _GP2)
	(setq is_90 (- (nth 3 _coords) (nth 2 _coords)))
	(if (= GK_ABSOLUTE 0)
		;;;;;;;;;;;; то
		(progn
			(setq _X (strcat _X "@"))
			(setq _Z (strcat _Z "@"))
		)
	)
	(if (equal (/ pi 2) is_90 0.1)
		(progn
			(if (= _G "G2*")
					(setq _G "G12*")
					(setq _G "G13*")
			)
			(setq res (list 
								_G 
								(strcat "X" _X "*") 
								(strcat "Z" _Z) 
			))
		)
		;;;;;;;;;;; иначе
		(progn
			(setq _GP1 (round _GP1 3))
			(setq _GP2 (round _GP2 3))
			(setq _GP1 (coord_to_string _GP1))
			(setq _GP2 (coord_to_string _GP2))
			(if (= GK_NOT_USE_P 1)
				(progn
					(setq _GP1 "")
					(setq _GP2 "")
				)
			)
			(if (= GK_ORDER "right")
				(setq res (list
									_G 
									(strcat "X" _X "*") 
									(strcat "Z" _Z "*") 
									(strcat "P" _GP1 "*")
									(strcat "P" _GP2)
							)
				)
				; иначе, если порядок отрисовки идет слева
				(setq res (list
									_G 
									(strcat "X" _X "*") 
									(strcat "Z" _Z "*") 
									(strcat "P" _GP2 "*")
									(strcat "P" _GP1)
							)
				)
			)
				
		)
	)
)

;###################################################################################
; функция настройки %)

(defun get_from_nc31_dialog()
	(setq GK_USE_45 (atoi (get_tile	"t_45")) )
	(setq GK_45_AXIS (get_tile	"rb_X")) 
	(if (= GK_45_AXIS "1")
		(setq GK_45_AXIS "X")
		(setq GK_45_AXIS "Z")
	)
	(setq GK_USE_F (atoi (get_tile	"t_F")) )
	(setq GK_F (atoi (get_tile	"eb_F")) )
	(setq GK_USE_~ (atoi (get_tile	"t_~")) )
	(setq GK_NOT_USE_P (atoi (get_tile	"t_argum_p")) )
	(setq GK_CP_NAME  (get_tile	"eb_file_name"))
)

;###################################################################################
; вывод координат в G -кодах

(defun print_to_G_code(list_of_coord   revers_order / _tmp	 _last	_n 	_text_X		 _text_Z 		 _G_CODE 	_coords_for_print _res 	_lay_name)
	(setq _G_CODE  NIL)
	(setq _lay_name (get_current_layer))
	(setq _G_CODE (list (strcat "T" (last _lay_name))))
	(setq _tmp (car GK_LIST_OF_COORD))
	(setq _last _tmp)
	(setq _text_X (strcat  
				"X" (coord_to_string (*  2 (car _last)  ))))

	(setq _text_Z (strcat 
				"Z" (coord_to_string  (nth 1 _last) ) 		))
	
	;  подходить ли на ускоренной подаче к первой точке?
	(if (= GK_USE_~ 1)
		(progn
			(setq _text_X (strcat  _text_X "~"))
			(setq _text_Z (strcat 	_text_Z 	"~"))			
		)
		(setq _text_X (strcat _text_X "*"))
	)
	(setq _G_CODE (append _G_CODE (list _text_X 		_text_Z)	))
	(setq _n 1)
	(while (< _n (LENGTH list_of_coord ))
					; размеры, которые будут отображаться в программе с G-кодами
	
					(setq _coords_for_print(list 
												(nth _n list_of_coord)  
												(nth (- _n 1) list_of_coord)
													))
				
		(if	(or 	(and (> (LENGTH (nth (- _n 1)  GK_LIST_OF_COORD)) 2) (= revers_order 0)) 
						(and (> (LENGTH (nth  _n  GK_LIST_OF_COORD)) 2) (= revers_order 1) ) )
						
			(progn		; нахождение дуги
				(if (= revers_order 1)
						(setq _tmp (make_g2_3_g12_13 	 
												(nth _n GK_LIST_OF_COORD)  
												(nth (- _n 1) GK_LIST_OF_COORD)  
												1 
												_coords_for_print
												))
						(setq _tmp (make_g2_3_g12_13 	 
												(nth (- _n 1) GK_LIST_OF_COORD)  
												(nth _n GK_LIST_OF_COORD)  
												0 
												_coords_for_print
												))
				)
				(setq _G_CODE (append _G_CODE _tmp))

				
			)
			(progn		; нахождение прямолинейных участков
				(setq _angle (ANGLE  
						(nth (- _n 1) GK_LIST_OF_COORD) 
						(nth _n GK_LIST_OF_COORD)
					))
				(setq _angle (/ (* _angle 180) pi))
				(if (and (or  (equal _angle 135 0.1) (equal _angle 45 0.1) (equal _angle 225 0.1) (equal _angle 315 0.1))
								(= GK_USE_45 1 ))
						(progn	; нахождение фасок
								(setq _tmp (make_45 (nth _n list_of_coord) _angle))
								(setq _G_CODE (append _G_CODE 	_tmp))
						)
						(progn	; построение обычных прямолинейных участков
							(setq _text_X "")
							(setq _text_Z "")	
							(setq _tmp (nth _n list_of_coord))
							(if  (= GK_ABSOLUTE 1)
								(progn
									(if (not (equal (car _last) (car _tmp) 0.001))
											(setq _text_X (strcat "X" (coord_to_string (*  2 (car _tmp ) ))))
									)
									(if (not (equal (nth 1 _last) (nth 1 _tmp) 0.001))
											(setq _text_Z (strcat "Z" (coord_to_string  (nth 1 _tmp) )))
									)
								)
								(progn
									(if (/= (car _tmp) 0)
											(setq _text_X (strcat "X" (coord_to_string (*  2  (car _tmp ) ))))
									)
									(if (/= (nth 1 _tmp) 0)
											(setq _text_Z (strcat "Z" (coord_to_string (nth 1 _tmp)  )))
									)
								)
							)
							(if ( = GK_ABSOLUTE 0)
									(progn
										(setq _text_X (strcat _text_X "@"))
										(setq _text_Z (strcat _text_Z "@"))
									)
							)
							(if (and (> (strlen _text_X) 1) (> (strlen _text_Z) 1))
								(setq _text_X (strcat _text_X "*"))
							)
							
							
							(if (> (strlen _text_X ) 	1 )
									(setq _G_CODE (append _G_CODE (list _text_X)))
							)
							(if (> (strlen _text_Z ) 	1)
									(setq _G_CODE (append _G_CODE (list _text_Z)))
							)
						)
				
				)
			)
		)

		(setq _last (nth _n list_of_coord))
		(setq _n (+ _n 1))
	
	)
	(setq _res _G_CODE)
)

;###################################################################################
; функции доступные из коммандной строки AutoCAD

(defun C:GK-NC31-PREF( / _dcl_id _dcl_file) ; настройка УП на вывод в файл
	(setq _dcl_file "c:/gk_autocad/gk_dialog.dcl")
	(setq _dcl_id (load_dialog _dcl_file))
	(if (not (new_dialog "gk_nc31_dialog" _dcl_id))
		(exit)
	)
	(princ "here")
	(action_tile "accept"	"(get_from_nc31_dialog) (done_dialog)")
	(action_tile "cancel"	"(done_dialog)")
	(start_dialog)
	(unload_dialog _dcl_id)
)

(defun C:GK-NC31-MULTI-POLY-SPESIAL( / _fake _lst_point _tmp _tmp_print 	_r _r _index) 
	; функция для тех случаев, когда нужно учитывать радиус инструмента прямо,
	; без использования поправки на радиус 
	
	;
	(setq sys_var (mapcar 'getvar '("osmode" "cmdecho")))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	;;;;;;;

	(setq GK_COLOR 1 )
	(setq _lst_point NIL)
	(setq _tmp_print NIL)
	(setq _index (getint "Введите индекс, с которого нужно нумеровать:<1>"))
	(setq _side (getint "Введите рабочую четверть:<2>"))
	(setq _r (getreal "Введите радиус инструмента:<0.8>"))
	(if (= _r nil)
		(setq _r 0.8)
	)
	(if (= _index nil)
		(setq _index 1)
	)
	(if (= _side nil )
		(setq _side 2)
	)
	
	(while (/= (setq ent (car (entsel "\nВыберите объекты (точка либо полилиния): ")))	NIL)
		(setq ent2 (entget ent))
		(setq _tmp (from_object_to_coord ent2 ))
		(setq _lst_point (append  _lst_point (list _tmp )))
		(setq _tmp_print (append _tmp_print _tmp))
	)

	(setq  _tmp_print (find_real_coord _tmp_print "x/2-z"))
	(setq GK_LIST_OF_COORD _tmp_print)
	(if (> (nth 1 (car _tmp_print))	(nth 1 (last _tmp_print)))
		(setq GK_ORDER "right")
		(setq GK_ORDER "left")  
	)
	(draw_circle_and_text _tmp_print 	_r	 _index  "x/2-z" _side)
	(setq gk_rev 0)
	(setq answ (getstring "\nСделать обратный порядок точек? <Д/Н>"))
	(if ( OR ( = answ "д" ) ( = answ "y" ) )
		(progn
			(command "ОТМЕНИТЬ"  1)
			(setq _tmp_print (revers_order_of_coord _tmp_print))
			(draw_circle_and_text  _tmp_print 	_r _index 	"x/2-z" _side)
			(setq gk_rev 1)
			(setq GK_LIST_OF_COORD _tmp_print)
		)
	)
	
	(setq GK_ABSOLUTE 1)
	(setq _fake NIL)
	(foreach _tmp _tmp_print
		(cond 
			((= _side 1) ; 1 четверть
				(setq _tmp (list
										(- (car _tmp) _r)
										(+ (nth 1 _tmp) _r)
										))
			)
			((= _side 2) ; 2 четверть
				(setq _tmp (list
										(- (car _tmp) _r)
										(- (nth 1 _tmp) _r)
										))
			)
			((= _side 3) ;  3 четверть
				(setq _tmp (list
										(+ (car _tmp) _r)
										(- (nth 1 _tmp) _r)
										))
			)
			((= _side 4) ; 4 четверть
				(setq _tmp (list
										(+ (car _tmp) _r)
										(+ (nth 1 _tmp) _r)
										))
			)		
		)		
		(setq _fake (append _fake (list _tmp)))		
	)
	(setq _lst_point _fake )
	(setq GK_LIST_OF_COORD _lst_point)
	
	(print_to_table _lst_point "Абсолютные значения" "x/2-z" _index) 
	
	(setq  GK_USE_45 0 )
	(setq GK_USE_~ 0)
	
	(setq myl (print_to_G_code _lst_point    gk_rev))
	
	(print_to_clipboard myl 0)
	;;;;;;;
	(mapcar 'setvar '("osmode" "cmdecho") sys_var)
	(setq _res (list _rev_order _lst_point))
)

(defun C:GK-NC31-MULTI-POLY( / ent _lst_point  _tmp _tmp_print	index  _G_code)
	; 	преобразование нескольких полилиний в связный G код
	(setq GK_COLOR 1 )
	(setq _lst_point NIL)
	(setq _tmp_print NIL)
	(setq _G_code NIL)
	(setq index 1)
	(while (/= (setq ent (car (entsel "\nВыберите объекты (точка либо полилиния): ")))	NIL)
		(setq ent2 (entget ent))
		(setq _tmp (from_object_to_coord ent2 ))
		(setq _tmp (draw_all _tmp index "x/2-z"))
		(setq gk_rev (car _tmp))
		(setq _tmp (last _tmp))
		(setq _G_code (append _G_code (print_to_G_code _tmp gk_rev)))
		(setq _tmp_print (append _tmp_print _tmp))
		(setq index (+ index (LENGTH _tmp)))
	)
	(print_to_table _tmp_print "Абсолютные значения" "x/2-z" 1) 
	(print_to_clipboard _G_code 0)
	(princ)
)

(defun C:GK-NC31-POLY( / ent n lst_point lst_point_dif 	_answ) ; разбор ОДНОЙ полилинии
	(setq GK_COLOR 1 )
	(setq ent (car (entsel "\nВыберите полилинию: ")))
	(setq ent2 (entget ent))
	(setq lst_point (from_object_to_coord ent2 ))
	
	(setq lst_point (draw_all lst_point 1 "x/2-z"))
	(setq gk_rev (car lst_point))
	(setq lst_point (last lst_point))
		
	(print_to_table lst_point "Абсолютные значения" "x/2-z" 1) 
	
	(setq lst_point_dif (mk_difference lst_point))
	(print_to_table lst_point_dif "Относительные значения" "x/2-z" 1) 
	(setq _answ (getstring "\nОтстроить G-код в абсолютных значениях? <Д/Н>"))
	(if (or (= _answ "Д") ( = _answ "д"))
		(progn
			(setq GK_ABSOLUTE 1)
			(setq myl (print_to_G_code  lst_point   gk_rev))
			(print_to_clipboard myl 0)
		)
		(progn
			(setq GK_ABSOLUTE 0)
			(setq myl (print_to_G_code  lst_point_dif   gk_rev))
			(print_to_clipboard myl 0)
		)
	
	)
	(princ)
)

(defun C:GK-NC31-MAKE-CHART(/ _layers _T_layers _T _lay _weigth 	_w   _undo_tmp  _sys_var _material  _p1 _p2  _info)
	; 	построение листа "на печать" составляется компиляцией из слоев 
	;	"заготовка"+"деталь"+"Т-..."
	;	все примитивы также переводятся в цвет по слою.
	
	(vla-startundomark
		(setq _undo_tmp (vla-get-activedocument (vlax-get-acad-object)))
    ) ; 
	
	(setq _sys_var (mapcar 'getvar '("osmode" "cmdecho")))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	;;;;;
	(setq GK_COLOR 1 )
	
	(setq _layers (get_layers_names))
	(setq _T_layers (is_T _layers))
	(setq _weigth nil)
	(command "_LAYER" "_M" "на печать" "")
	(command "_LAYER" "_S" "0" "")
	(command "_LAYER" "_OFF" "на печать" "")
	(setq _p1 (getpoint "Укажите ширину меж схемами (точка 1)"))
	(setq _p2 (getpoint "Укажите ширину меж схемами (точка 2)"))
	(setq _weigth ( 
							+
							(abs(nth 1 _p1)) 
							(abs(nth 1 _p2))
	
	))
	(setq _w 0)
	(setq _tmp_list nil)
	(foreach _T _T_layers
		(setq _tmp_list (append _tmp_list  
								(list  
								(atoi (substr _T  3 3))
								)
								))
	
	)
	(setq _tmp_list (sort_int _tmp_list))
	(setq _T_layers nil)
	(foreach _T _tmp_list
		(setq _T_layers (append _T_layers  
								(list 
								(strcat "T-" (itoa _T))
								)
								))	
	)
	(foreach _T 		_T_layers
		(foreach  _lay 	_layers
			(command "_LAYER" "_OFF" _lay "")
		)
		(command "_LAYER" "_ON" "деталь" "")
		(command "_LAYER" "_ON" "заготовка" "")
		(command "_LAYER" "_ON" _T  "")
		(copy_to_layer "на печать" _w)	
		(setq _w (+ _w _weigth))
	)

	(foreach  _lay 	_layers
		(command "_LAYER" "_OFF" _lay "")
	)
	(command "_LAYER" "_M" "на печать" "")
	(setq _w 0)
	(foreach _T _T_layers
		(command "_text" (list -3 (+ 3 _w))  90 _T)
		(setq _w (+ _w _weigth))
	)
	(setq _material (ssget "_w" (list  -1200  -1200) (list  1200 1200)))
	(setq _info (get_all_info))
	(setq _name (strcat (car _info) " " (nth 1 _info) ".   Операция: " (nth 2 _info) "  (Дата:  " (get_date) ")"))
	(command "_text" (list -7 7) 90 _name)
	(command "_ddptype")
	;;;;;;
	(mapcar 'setvar '("osmode" "cmdecho") _sys_var)
	(vla-endundomark _undo_tmp) 
)

(defun c:GK-NC31-CONVERT-TABLE (/ obj)
	; 	преобразование таблицы вида "	№ точки 	|	 Х	 | 	Z		" из абсолютных значений
	;	в относительные.
	(setq GK_COLOR 1 )
	(vl-load-com)
	(if (setq obj (car (entsel)))
		(progn
			(setq obj (vlax-ename->vla-object obj))
			(if (= (vla-get-ObjectName obj) "AcDbTable")
				(progn
					(setq _i 2)
					(setq _kord nil)
					(while (< _i (vla-get-rows obj)); FIXME:	 отсутствие ф-ции в bricscad4linux
						(setq _x (atof (vla-GetText obj _i  1)))
						(setq _z (atof (vla-GetText obj _i 2)))
						(setq _kord (append _kord  (list (list _x _z) )))
						(setq _i (+ _i 1))
					)
				)
			)
		)
	)
  (setq _kord (mk_difference _kord))
  (print_to_table _kord   "Относительные значения"    "x/2-z-spesial"  (atoi (vla-GetText obj 2 0)) )
)
