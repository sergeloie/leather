(defun c:STITCHARC (/ m k n side p0 dirIn dirOut angIn angOut step sign p pts ang)

  ;; ФАКТИЧЕСКОЕ количество стежков на закруглении
  (setq m (getint "\nФактическое количество стежков на повороте (90°): "))
  (if (<= m 0)
    (progn
      (princ "\nОшибка: количество стежков должно быть больше 0.")
      (exit)
    )
  )

  ;; внутреннее k (с учётом прямого сегмента)
  (setq k (1+ m))
  (setq n (* 4 k))

  ;; длина стежка
  (setq side (getdist "\nШаг стежка: "))

  ;; направление входа
  (setq dirIn
    (getint
      "\nНаправление входящего шва [1-вправо 2-вверх 3-влево 4-вниз]: "
    )
  )

  ;; направление выхода
  (setq dirOut
    (getint
      "\nНаправление выходящего шва [1-вправо 2-вверх 3-влево 4-вниз]: "
    )
  )

  ;; проверка перпендикулярности
  (if (/= (rem (abs (- dirIn dirOut)) 2) 1)
    (progn
      (princ "\nОшибка: швы должны быть перпендикулярны.")
      (exit)
    )
  )

  ;; точка начала закругления
  (setq p0 (getpoint "\nТочка начала закругления: "))

  ;; углы направлений
  (setq angIn
    (cond
      ((= dirIn 1) 0.0)
      ((= dirIn 2) (/ pi 2))
      ((= dirIn 3) pi)
      ((= dirIn 4) (* 3 (/ pi 2)))
    )
  )

  (setq angOut
    (cond
      ((= dirOut 1) 0.0)
      ((= dirOut 2) (/ pi 2))
      ((= dirOut 3) pi)
      ((= dirOut 4) (* 3 (/ pi 2)))
    )
  )

  ;; шаг поворота
  (setq step (/ (* 2 pi) n))

  ;; направление поворота (влево / вправо)
  (setq sign
    (if (> (sin (- angOut angIn)) 0) 1 -1)
  )

  ;; старт после прямого сегмента
  (setq p p0)
  (setq pts (list p))
  (setq ang (+ angIn (* sign step)))

  ;; строим РОВНО m стежков
  (repeat m
    (setq p (polar p ang side))
    (setq pts (append pts (list p)))
    (setq ang (+ ang (* sign step)))
  )

  ;; рисуем полилинию
  (command "_.PLINE")
  (foreach pt pts (command pt))
  (command "")

  (princ)
)
