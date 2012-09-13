
gk_nc31_dialog : dialog {
 label = "Параметры построения программы";
 :column{
	alignment = right;
	fixed_width=true; //Минимальная ширина по объектам внутри
	:column{
	
		label = "Основные :";
		:toggle{
			label="Не использовать функции +/-45";
			key = "t_45";
			value = "0";
			}
		:row{
			alignment = left;
			:text{
				label="Выдерживание размера для фаски по оси";
				}
			:radio_column{
				:radio_button{
					label = "Х";
					key = "rb_X";
					value = "1";
					}
				:radio_button{
					label = "Z";
					key = "rb_Z";
					}
				}
			}

				
		:row{
			alignment = left;
			:toggle{
				label="Подача, используемая для фаски - F " ;
				key="t_F";
				}
			:edit_box{
				alignment = left;
				value="0";
				key="eb_F";
				
				}
			}
		:toggle{
			label="Подойти на ускоренной подаче к 1 точке в траектории";
			key="t_~";
			value="1";
			}
		:toggle{
			label="Пропускать аргументы 'P' в функциях G2/G3";
			key="t_argum_p";
			value="0";
			}
	}
		
	:spacer{}
	:column{
		label="Дополнительные :";
		:row{
			:text{
				label="Имя файла для сохранения УП ";
				}
			
			:edit_box{
				value="УП.xls";
				key="eb_file_name";
				}
			}
		:spacer{}
		}

	ok_cancel;
	}
}
