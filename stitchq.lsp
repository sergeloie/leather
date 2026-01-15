(defun c:STITCHQ ( / ent obj len dist pt oldos oldlay
                     deriv ang bladeAng half p1 p2
                     doc layers)

  (vl-load-com)

  ;; === ПРЕСЕТЫ ТИХОГО РЕЖИМА ===
  (setq bladeAng (/ pi 4.0)) ; 45°
  (setq half 1.0)            ; длина жала 2 мм
  (setq step 4.0)            ; шаг 4 мм
  (setq offset 0.0)          ; отступ 0 мм

  ;; === Документ и слой ===
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))

  ;; Сохраняем текущий слой
  (setq oldlay (getvar "CLAYER"))

  ;; Проверяем / создаём слой 3_HOLES
  (if (not (tblsearch "LAYER" "3_HOLES"))
    (vla-add layers "3_HOLES")
  )

  ;; Делаем слой текущим
  (setvar "CLAYER" "3_HOLES")

  ;; === Object Snap OFF ===
  (setq oldos (getvar "OSMODE"))
  (setvar "OSMODE" 0)

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

  ;; === Расстановка пробоев ===
  (setq dist offset)
  (while (<= dist len)
    (setq pt (vlax-curve-getPointAtDist ent dist))

    ;; касательная
    (setq deriv (vlax-curve-getFirstDeriv
                  ent
                  (vlax-curve-getParamAtDist ent dist)
                ))
    (setq ang (+ (atan (cadr deriv) (car deriv)) bladeAng))

    ;; жало
    (setq p1 (polar pt ang half))
    (setq p2 (polar pt (+ ang pi) half))

    (command "_.LINE" "_non" p1 "_non" p2 "")

    (setq dist (+ dist step))
  )

  ;; === Возврат настроек ===
  (setvar "OSMODE" oldos)
  (setvar "CLAYER" oldlay)

  (princ "\nТихий режим: пробои готовы.")
  (princ)
)
