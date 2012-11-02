number_cp : dialog {
 label = "Параметры...";
//		fixed_width=true;
//		fixed_height=true;
		alignment = left;
		fixed_width = true;
	:column{
		fixed_width=true;
		fixed_height=true;
		label = "Укажите номер кадра, с которого";
		:text{
			label=" нужно вставлять УП в файл Excel:";
			fixed_width = true;
			}
		alignment = left;
		:edit_box{
			alignment = centered;
			value = 0;
			key = "eb_numner_cp";
			fixed_width = true;
			}
		
		ok_cancel;
		}
	
}