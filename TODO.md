# TODOs

- glue funktioniert nicht wie intendiert: glue verbindet die Actions intrinsich, die einzelnen Wirkungen der Actions tauchen also nicht auf, sondern nur die letzte.

type alias Modifier model = model -> model

type Operation model error =«»
                 Pure  (Modifier model)
               | Async (model -> Task error (Modifier model))

- Reduziere die Operatoren; jetzt mit `glue` sollte vieles reduziert werden können. Braucht man die incorporates überhaupt noch? Oder reicht ein "onsuccess" ? Sonst ist zu viel Gemenge von Modifier und Actions.
- Schreibe die incorpr um: hier soll ausgeführt werden unmittelbar nach dem errorHandler.
- Es gibt noch einen semantischen Fehler in der momentanen JsonEcho
