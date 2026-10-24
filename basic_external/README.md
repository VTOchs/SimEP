# Basic External

Eine eigenständige, bewusst einfache Shiny-App für externe Nutzer.

## Funktionen

- ein allgemeiner Reiter „Eingabe“
- ein Freitextfeld für den Antrag
- Eingaben für Ja, Nein und Enthaltung
- Berechnung des Ergebnisses über den Button „Ergebnis“
- ein allgemeiner Reiter „Ergebnis“
- der Antragstext ist optional; für ein Ergebnis müssen nur alle drei Stimmenwerte angegeben werden
- ein Donutdiagramm mit Ja-/Nein-Stimmen und Enthaltungen als Angabe in der Diagrammmitte
- der Antragstext wird im Ergebnis direkt aktualisiert
- ein Zurücksetzen-Button leert Eingaben und Ergebnis

Die App verwendet keine Dateien, Funktionen oder API-Verbindungen der internen SimEP-App. Ergebnisse werden nur für die jeweilige Browser-Sitzung im Arbeitsspeicher gehalten und nicht dauerhaft gespeichert.

## Lokal starten

Im Ordner `basic_external` in R oder RStudio ausführen:

```r
shiny::runApp()
```

Es werden die R-Pakete `shiny` und `ggplot2` benötigt.

## Deployment

Der Ordner `basic_external` kann direkt über Posit Publisher in VS Code auf Posit Connect Cloud veröffentlicht werden. Die Startdatei ist `app.R`; `renv.lock` legt die benötigten R-Pakete reproduzierbar fest.
