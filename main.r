volna = function(field,ax,ay,bx,by) {
  # ax, ay - это координаты начальной точки;
  # bx, by - это координаты конечной точки.
  # KAM - условное значение для камня;
  # CLEAN - условное значение для свободной клетки.
  KAM =-1
  CLEAN =-2
  
  # Векторы для определения направления движения вверх, вправо, вниз, влево.
  dx<-c(1,0,-1,0)
  dy<-c(0,1,0,-1)
  
  
  # Проверка наличия камня в начальной или конечной точке.
  if ((field[ax,ay]==KAM) || (field[bx,by]==KAM))
    return(print('Вы в камне'))
  
  
  # Устанавливаем начальную точку и счетчик шагов.
  step=0
  field[ax,ay]=step
  
  num_col=ncol(field)
  num_rows=nrow(field)
  
  stop=FALSE # создаем флаг
  
  while (!stop){
    stop=TRUE
    
    for (y in 1:num_col){
      for (x in 1:num_rows){
        # Ищем клетки, в которые можно перейти.
        if (field[x,y]==step){ # попадаем в клетку настоящего шага
          
          for (num_direction in 1:4){ # Рассматриваем различные направления движения
            ix=x+dx[num_direction]
            iy=y+dy[num_direction]
            if (ix>=1 && ix<=num_rows && iy>=1 && iy<=num_col && field[ix,iy]==CLEAN){
              stop=FALSE
              # Обновляем точку в матрице как номер шага
              field[ix,iy]=step+1
            }
          }
        }
      }
    }
    # Увеличиваем шаг после каждого
    step=step+1
  }
  
  # Если до конечной точки не дойти из-за камней - возвращаем FALSE.
  if (field[bx,by]==CLEAN)
    return(FALSE)
  print(field)
  
  # Находим кратчайший путь от начальной точки до конечной.
  path_len=field[bx,by]
  x=bx
  y=by
  step=path_len
  # Создаем массивы для хранения координат пути.
  px<-array(dim=c(1))
  py<-array(dim=c(1))
  while (step>0)
  {
    px[step+1]=x
    py[step+1]=y
    step=step-1
    
    for (num_direction in 1:4){
      ix=x+dx[num_direction]
      iy=y+dy[num_direction]
      if (ix>=1 && ix<=num_rows && iy>=1 && iy<=num_col && field[ix,iy]==step){
        x=x+dx[num_direction]
        y=y+dy[num_direction]
        break
      }
    }
    
  }
  
  # Добавляем координаты начальной точки в массивы.
  px[1]=ax
  py[1]=ay
  #print(px)
  #print(py)
  
  # Помечаем путь нулями на поле.
  for (i in 1:(path_len+1)) {
    field[px[i],py[i]]=0
  }
  print('Матрица с путём из нулей:')
  print(field)
  
  plot(x, y, xlim = c(1, dim(field)[2]), ylim = c(dim(field)[1], 1))
  for (x in 1:dim(field)[2]) {
    for (y in 1:dim(field)[1]) {
      if (field[y, x] == -1) {
        yx = c("K") #Kamen
        points(x, y, pch = yx, col = "red")
      }
      if (field[y, x] == 0) {
        yx = c("P") # Path
        points(x, y, pch = yx, col = "green")
      }
      if (field[y, x] > 0) {
        yx = c("0") # Empty cell
        points(x, y, pch = yx)
      }
    }
  }
  
  # рисуем start_point and end_point
  yx = c("S")
  points(ay, ax, pch = yx, col = "blue")
  yx = c("E")
  points(by, bx, pch = yx, col = "blue")
  
  
  return(TRUE)
}

mat <- c(-2,-2,-1,-2,-2,-2,-2,-2,-2,-2,-2,-2,
         -2,-2,-1,-2,-2,-1,-2,-2,-2,-2,-2,-2,
         -2,-2,-2,-2,-2,-1,-2,-2,-2,-2,-2,-2,
         -2,-2,-2,-2,-2,-2,-2,-2,-1,-1,-2,-2,
         -2,-1,-1,-1,-1,-2,-2,-2,-2,-2,-2,-2,
         -2,-2,-2,-2,-2,-2,-2,-2,-2,-1,-1,-2,
         -2,-2,-2,-2,-2,-2,-2,-2,-2,-1,-1,-2,
         -2,-2,-2,-2,-2,-2,-2,-2,-1,-2,-2,-2)
pole <- matrix(mat,8,12,byrow=TRUE)
print(pole)


volna(pole,8,12,5,1)