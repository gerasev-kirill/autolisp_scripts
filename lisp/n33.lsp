;###################################################################################
;	Программа преобразования полилиний в таблицы с координатами в "изломах"
;		* вывод в acad таблицах,
;		* вывод в xls-файлы,
;		* вывод в G-кодах.
;###################################################################################
;	Для фрезерных станков с системой координат X-Z.
;	Стойка Н33-2М
;	Герасев Кирилл
;	01/01/2013
;###################################################################################
;

;###################################################################################
; загрузка модуля с вспомогательными функциями

(load "main_module")


(defun C:GK-N33-POLY( / _lst_point	_sys_var)
	;  
	(setq _sys_var (mapcar 'getvar '("osmode" "cmdecho")))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	;;;;;;;
	
	(setq GK_COLOR 5 )
	(setq ent (car (entsel "\nВыберите полилинию: ")))
	(setq ent2 (entget ent))
	(setq _lst_point (from_object_to_coord ent2 ))
	(setq  _lst_point (find_real_coord _lst_point "x-z"))
	
	(if (> (nth 1 (car _lst_point))	(nth 1 (last _lst_point)))
		(setq GK_ORDER "right")
		(setq GK_ORDER "left")  
	)
  
	(setq _lst_point (append (list (list 0 0)) _lst_point))
	(draw_point_and_text _lst_point "x-z" 0)
	(setq answ (getstring "\nСделать обратный порядок точек? <Д/Н>"))
	(if ( OR ( = answ "y" ) ( = answ "д" ) )
			(progn
				(command "_undo"  1)
				(setq _lst_point (revers_order_of_coord _lst_point))
				(draw_point_and_text _lst_point  "x-z" 0)
				(setq GK_LIST_OF_COORD _lst_point)
			)
		)
	(setq GK_ABSOLUTE 1)
	(setq _lst_point (append  _lst_point   (list (nth 1 _lst_point))))
	(print_to_table _lst_point "Абсолютные значения" "x-z" 0)
	
	;;;;;;;
	(mapcar 'setvar '("osmode" "cmdecho") sys_var)
	(princ)
)
