Funzioni:
- (json-parse json)
 > Carica in una forma lisp-friendly un oggetto
   JSON da una stringa

- (json-get list fields)
 > Dato una lista JSON e una serie di “campi”
   recupera l’oggetto corrispondente.

- (json-load filename)
 > Carica l'oggetto JSON in una forma lisp-friendly

- (json-write list filename)
 > Salva l'oggetto JSON in un file