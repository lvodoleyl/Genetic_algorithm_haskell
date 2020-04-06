from tkinter import *
from tkinter import ttk, messagebox
import matplotlib.pyplot as plt
from tkinter.ttk import Combobox, Checkbutton
import sympy as s
from math import *
import subprocess
import time

def run_app(event):
    try:
        _func = func.get()
        _down = float(down_bord.get())
        _up = float(up_bord.get())
        _delta = float(delta.get())
        _popul = int(popul.get())
        _type_select = select_list.get()
        _type_cross = cross_list.get()
        _cross_m = int(cross_n.get())
        _cross_p = float(cross_p.get())
        _mutat_p = float(mut_p.get())
        _type_end = end.get()
        _end_n = int(end_n.get())
    except:
        messagebox.showinfo('Неправильный ввод', 'Неправильный формат данных, или не все поля заполнены!')
        return 
    try:
        _func_py = s.S(_func)
        a = _func_py.subs('x', 3)
        float(a)
    except:
        messagebox.showinfo('Неправильный ввод', 'Неправильно описана функция. Используйте наименование аргумента, как "x"!')
        return
    #print(_func, _down, _up, _delta, _popul, _type_select, _type_cross, 
    #    _cross_m, _cross_p, _mutat_p, _type_end,_end_n)
    f = open('genetic_alg.txt', 'r')
    x = [_down+(_up - _down)/1000*i for i in range(1000)]
    root.update()
    y = [_func_py.subs('x', i) for i in x]
    root.update()
    if chk_state.get():
        tab_multiplexor.select(1)
        for line in f:
            time.sleep(0.3)
            if line.split(" ")[0] != "Result:":
                list_vis_x = []
                list_vis_y = []
                list_arg_res = line.split(" ")
                for arg_res in list_arg_res:
                    try:
                        tmp = arg_res.split(";")
                        list_vis_x.append(float(tmp[0]))
                        list_vis_y.append(float(tmp[1]))
                    except:
                        continue
                fig, ax = plt.subplots(figsize=(4.6,2.5))
                ax.set_title(_func)
                #ax.set_title('Генетический алгоритм')
                #ax.set_xlabel('x')
                #ax.set_ylabel(_func)
                ax.grid()      # включение отображение сетки
                ax.plot(x, y)
                ax.scatter(x = list_vis_x, y = list_vis_y, color="red")
                print(list_vis_x)
                print(list_vis_y)
                fig.savefig('gen_alg')
                im = PhotoImage(file='gen_alg.png')
                modeling = Label(tab_models, image=im)
                modeling.grid (row = 1, column = 1)
                root.update()
            else:
                tab_multiplexor.select(2)
                root.update()
                res = line.split(" ")[1]
                res_x, res_y = res.split(";")[0], res.split(";")[1]
                func_i = Label(tab_result, text = 'Функция f(x)='+_func, width = 55, height = 1,  fg = 'black',font='arial 10')
                func_i.grid(row = 1, column = 1)
                res_y_i = Label(tab_result, text = 'Максимальный найденный элемент f(x)='+res_y, width = 55, height = 1,  fg = 'black',font='arial 10')
                res_y_i.grid(row = 2, column = 1)
                res_x_i = Label(tab_result, text = 'Аргумент х='+res_x, width = 55, height = 1,  fg = 'black',font='arial 10')
                res_x_i.grid(row = 3, column = 1)
                file_i = Label(tab_result, text = 'Информация о работе алгоритма представлена в файле "genetic_alg.txt"', width = 55, height = 1,  fg = 'black',font='arial 10')
                file_i.grid(row = 4, column = 1)
                root.update()
    else:
        tab_multiplexor.select(2)
        for line in f:
            if line.split(" ")[0] != "Result:":
                continue
            root.update()
            res = line.split(" ")[1]
            res_x, res_y = res.split(";")[0], res.split(";")[1]
            func_i = Label(tab_result, text = 'Функция f(x)='+_func, width = 60, height = 1,  fg = 'black',font='arial 10')
            func_i.grid(row = 1, column = 1)
            res_y_i = Label(tab_result, text = 'Максимальный найденный элемент f(x)='+res_y, width = 40, height = 1,  fg = 'black',font='arial 10')
            res_y_i.grid(row = 2, column = 1)
            res_x_i = Label(tab_result, text = 'Аргумент х='+res_x, width = 60, height = 1,  fg = 'black',font='arial 10')
            res_x_i.grid(row = 3, column = 1)
            file_i = Label(tab_result, text = 'Информация о работе алгоритма представлена в файле "genetic_alg.txt"', width = 60, height = 1,  fg = 'black',font='arial 10')
            file_i.grid(row = 4, column = 1)
            root.update()
    f.close()
    return 

root = Tk()
root.title("Генетический алгоритм")
root.geometry("450x285")
tab_multiplexor = ttk.Notebook(root)
tab_parameters = ttk.Frame(tab_multiplexor)
tab_models = ttk.Frame(tab_multiplexor)
tab_result = ttk.Frame(tab_multiplexor)
tab_multiplexor.add(tab_parameters, text='Параметры')
tab_multiplexor.add(tab_models, text='Моделирование')
tab_multiplexor.add(tab_result, text='Результат')
beaut = Label(tab_parameters, text = 'Введите необходимые параметры ГА:', width=50,height=1,fg='black',font='arial 10')
beaut.grid(row = 1, column = 0, columnspan = 20)
func_info = Label(tab_parameters, text = 'Функция y = f(x) =  ', width = 14, height = 1,  fg = 'black',font='arial 10')
func_info.grid(row=2, column =1, columnspan = 3)
func = Entry (tab_parameters, width = 30)
func.grid (row = 2, column = 4,columnspan = 3)
down_bord_i = Label(tab_parameters, text = 'Аргументы от ', width = 10, height = 1,  fg = 'black',font='arial 10')
down_bord_i.grid(row=3, column = 1)
down_bord = Entry (tab_parameters, width = 5)
down_bord.grid (row = 3, column = 2)
up_bord_i = Label(tab_parameters, text = ' до ', width = 5, height = 1,  fg = 'black',font='arial 10')
up_bord_i.grid(row=3, column = 3)
up_bord = Entry (tab_parameters, width = 5)
up_bord.grid (row = 3, column = 4)
delta_info = Label(tab_parameters, text = ' с погреш. ', width = 7, height = 1,  fg = 'black',font='arial 10')
delta_info.grid(row=3, column = 5)
delta = Spinbox (tab_parameters, value = [i*0.001 for i in range(1,10001,10)], width=5)
delta.grid (row = 3, column = 6)
popul_info = Label(tab_parameters, text = 'Особей в популяции: ', width = 20, height = 1,  fg = 'black',font='arial 10')
popul_info.grid(row=4, column =1, columnspan = 3)
popul = Entry (tab_parameters, width = 5)
popul.grid (row = 4, column = 4)
all_select = ('Колесо рулетки','Стох.универ.выб.','Турнирный выбор','Ранговый выбор', 'Случайный выбор')
select_info = Label(tab_parameters, text = 'Тип селекции', width = 20, height = 1,  fg = 'black',font='arial 10')
select_info.grid(row=5, column =1, columnspan = 2)
select_list = Combobox(tab_parameters, width =20, height = 1, value = all_select)
select_list.current(0)
select_list.grid( row = 6, column = 1, columnspan = 2)
all_crossing = ('M-точечный','"Единый"')
cross_info = Label(tab_parameters, text = 'Тип кроссинговера', width = 20, height = 1,  fg = 'black',font='arial 10')
cross_info.grid(row=5, column =3, columnspan = 2)
cross_list = Combobox(tab_parameters, width =20, height = 5, value = all_crossing)
cross_list.current(0)
cross_list.grid( row = 6, column = 3, columnspan = 2)
cross_n_i = Label(tab_parameters, text = 'M: ', width = 2, height = 1,  fg = 'black',font='arial 10')
cross_n_i.grid(row=5, column =5)
cross_n = Spinbox (tab_parameters, from_= 1, to = 5, width=5)
cross_n.grid (row = 6, column = 5)
cross_p_i = Label(tab_parameters, text = 'Вероятность кроссингов.:', width = 20, height = 1,  fg = 'black',font='arial 10')
cross_p_i.grid(row=7, column =1, columnspan = 2)
cross_p = Spinbox (tab_parameters, value = [x*0.05 for x in range(6,21)], width=5)
cross_p.grid (row = 7, column = 3)
mut_p_i = Label(tab_parameters, text = 'Вероятн. мутации:', width = 13, height = 1,  fg = 'black',font='arial 10')
mut_p_i.grid(row=7, column =4, columnspan = 2)
mut_p = Spinbox (tab_parameters, value = [x*0.01 for x in range(1,21)], width=5)
mut_p.grid (row = 7, column = 6)
end_list = ["Без улучшений N популяций", "Произвести N популяций", "Где функция достигает N"]
end_info = Label(tab_parameters, text = 'Критерий остановки алгоритма', width = 27, height = 1,  fg = 'black',font='arial 10')
end_info.grid(row=8, column =1, columnspan = 4)
end = Combobox(tab_parameters, width =27, height = 1, value = end_list)
end.current(1)
end.grid(row = 9, column = 1, columnspan = 4)
end_n_i = Label(tab_parameters, text = 'N: ', width = 1, height = 1,  fg = 'black',font='arial 10')
end_n_i.grid(row=8, column =5)
end_n = Entry (tab_parameters, width=5)
end_n.grid (row = 9, column = 5)
chk_state = IntVar()  
chk_state.set(1) 
chk = Checkbutton(tab_parameters, text='Включить моделирование', var=chk_state,onvalue=1,offvalue=0)  
chk.grid(column=1, row=10, columnspan = 2, pady=10)  
run_gen_alg_button = Button(tab_parameters,  text = 'Запустить', width = 10, height = 2, bg = 'green',fg = 'black', font = 'arial 10')
run_gen_alg_button.grid(row=10, column = 5, columnspan = 3)
run_gen_alg_button.bind("<Button-1>", run_app)
tab_multiplexor.pack(expand=1, fill='both')  
root.mainloop()
