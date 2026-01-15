(defun c:STITCH ( / step offset ent obj len dist pt oldos oldlay
                    mode deriv ang bladeAng half p1 p2
                    doc layers)

  (vl-load-com)

  ;; === Документ и слои ===
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))

  ;; Сохраняем текущий слой
  (setq oldlay (getvar "CLAYER"))

  ;; Проверяем / создаём слой
  (if (not (tblsearch "LAYER" "3_HOLES"))
    (vla-add layers "3_HOLES")
  )

  ;; Делаем слой текущим
  (setvar "CLAYER" "3_HOLES")

  ;; === Object Snap OFF ===
  (setq oldos (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; === Выбор формы пробоя ===
  (initget "Circle Slash Point")
  (setq mode (getkword
    "\nФорма пробоя [Circle/Slash/Point] <Circle>: "
  ))
  (if (null mode)
    (setq mode "Circle")
  )

  ;; === Шаг пробоя ===
  (setq step (getdist "\nВведите шаг пробоя <4 мм>: "))
  (if (null step)
    (setq step 4.0)
  )
  (setq step (float step))

  ;; === Отступ от начала ===
  (setq offset (getdist "\nОтступ от начала линии <0>: "))
  (if (null offset)
    (setq offset 0.0)
  )
  (setq offset (float offset))

  ;; === Выбор линии шва ===
  (setq ent (car (entsel "\nВыберите линию шва: ")))
  (if (null ent)
    (progn
      (setvar "OSMODE" oldos)
      (setvar "CLAYER" oldlay)
      (exit)
    )
  )

  ;; VLA объект
  (setq obj (vlax-ename->vla-object ent))

  ;; Длина кривой
  (setq len (vla-get-Length obj))

  ;; Параметры жала
  (setq bladeAng (/ pi 4.0)) ; 45°
  (setq half 1.0)            ; длина жала 2 мм / 2

  ;; === Расстановка пробоев ===
  (setq dist offset)
  (while (<= dist len)
    (setq pt (vlax-curve-getPointAtDist ent dist))

    (cond
      ;; ---- КРУГ O1 мм ----
      ((= mode "Circle")
        (command "_.CIRCLE" "_non" pt 0.5)
      )

      ;; ---- КОСАЯ ЧЕРТА ПО КАСАТЕЛЬНОЙ ----
      ((= mode "Slash")
        (setq deriv (vlax-curve-getFirstDeriv
                      ent
                      (vlax-curve-getParamAtDist ent dist)
                    ))
        (setq ang (+ (atan (cadr deriv) (car deriv)) bladeAng))
        (setq p1 (polar pt ang half))
        (setq p2 (polar pt (+ ang pi) half))
        (command "_.LINE" "_non" p1 "_non" p2 "")
      )

      ;; ---- ТОЧКА ----
      ((= mode "Point")
        (command "_.POINT" "_non" pt)
      )
    )

    (setq dist (+ dist step))
  )

  ;; === Возврат настроек ===
  (setvar "OSMODE" oldos)
  (setvar "CLAYER" oldlay)

  (princ "\nПробои добавлены на слой 3_HOLES.")
  (princ)
)
