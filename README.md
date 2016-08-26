Dependencies
----------------
* Racket(obviously): [Racket download page](https://download.racket-lang.org/)
* "gregor" library: do "raco pkg install gregor" (say yes to dependencies)
  [Documentation for gregor library](https://docs.racket-lang.org/gregor/index.html)
* "html-writing" library: do "raco pkg install html-writing" (say yes to dependencies)
  [Documentation for html-writing library](https://docs.racket-lang.org/html-writing/index.html)
* "csv-reading" library: do "raco pkg install csv-reading" (say yes to dependencies)
  [Documentation for csv-reading library](https://docs.racket-lang.org/csv-reading/index.html)

How to use
------------
* TL;DR: Open a terminal. The program's command should be available through the terminal(type a few characters of the name, and tab two times to see if it comes up).
  In this case:  
      * for the bilingual calendar: `csv-to-html --bi -o path/to/output-file  path/to/events-csv path/to/locations-csv`  
      * for the english calendar: `csv-to-html --en -o path/to/output-file  path/to/events-csv path/to/locations-csv`   
      * for the french calendar: `csv-to-html --fr -o path/to/output-file  path/to/events-csv path/to/locations-csv`   
  If not, find the path to the directory of the program, and do `cd path/to/program/folder`. There should be an executable launcher in there.
  In this case:  
      * for the bilingual calendar: `./csv-to-html.exe --bi -o path/to/output-file  path/to/events-csv path/to/locations-csv`  
      * for the english calendar: `./csv-to-html.exe --en -o path/to/output-file  path/to/events-csv path/to/locations-csv`   
      * for the french calendar: `./csv-to-html.exe --fr -o path/to/output-file  path/to/events-csv path/to/locations-csv`   
  If there isn't, there should be a source file `csv-to-html.rkt`.
  In this case:  
      * for the bilingual calendar: `racket csv-to-html.rkt --bi -o path/to/output-file  path/to/events-csv path/to/locations-csv`  
      * for the english calendar: `racket csv-to-html.rkt --en -o path/to/output-file  path/to/events-csv path/to/locations-csv`   
      * for the french calendar: `racket csv-to-html.rkt --fr -o path/to/output-file  path/to/events-csv path/to/locations-csv`   
  * If the one of the files' path begins with a slash ("-"), you must double-quote the path(i.e. `"-path/to/file"` instead of `-path/to/file`)
  * The paths can be relative or absolute.
    * Relative: if in folder `/Users/wahkeungchan/folder-of-program` and the files are in `/Users/wahkeungchan/Downloads`,
      the relative paths of the files are `../Downloads/name-of-file`. The `..` goes back one folder.
    * Absolute: An absolute path begins with the root folder "/". 
      The absolute path to the "Downloads" folder is `/Users/wahkeunchan/Downloads`.
      Use command `pwd` to see the absolute path of the current directory.
    * To navigate folders using terminal, use `cd path/to/folder` command. 
      To list files and folders in current directory, use `ls`.
      To list files and folders of another directory, use `ls path/to/folder`.    
  * The "-o" option specifies the path of the output file. Without that option, the html is printed to the standard output(see below)
  
* To print to standard output(which you can redirect to a file with `> filename` or pipe into another program with `| programname ...`),
  simply ommit the "-o" flag:
  `csv-to-html path/to/events-csv path/to/locations-csv`
* To see all options and help: `csv-to-html -h`
* To print default values of parameters(including fields names): `csv-to-html --print-defaults`
* To change date format: `csv-to-html -d date-format-string` 
  (see [this Link](http://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table) for specifications on valid date and time format syntax).
  The date format string must match the format of the dates in the event's csv
  * Example of valid date format strings: 
    - "M/d/yyyy" (example of date matching this format: "1/2/1993", "12/23/2012")
    - "MM/dd/yyyy" (example of dates matching this format: "01/01/2016", "12/04/2005")
    - "yyyy-MM-dd" (example of dates matching this format: "2016-01-01", "2005-04-12")
  
* To change time format: `csv-to-html -t time-format-string` 
  (see [this Link](http://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table) 
  for specifications on valid date and time format syntax).
  The time format string must match the format of the times in the event's csv. The format of the time strings in the csv must be consistent.
  * Example of valid date format strings: 
    - "HH:mm:ss" (example of times matching this format: "14:12:00", "04:04:04")
* To change values of fields' names: `csv-to-html -f s-expression-for-fields-names` 
  where 's-expression-for-fields-names' is a double-quoted string with this syntax: `"((field . |field-name|) ...)"`
  * Each field specification is in a pair of parentheses, 
  * the field and the field's name are separated with spaces and a dot: `(field . |fieldname|)`.__
  * The `|` characters before and after the field name are to escape the field name, 
    in case it contains special characters. 
  * Examples: `"((start-date . |_event_start_date|) (start-time . |_event_start_time|))"`
  * Alternatively, you can change the fields names of the csv so that they match the defaults of the program.
  * Also alternatively, you can change the default values of the program itself.
* To change default values for fields which might be missing: `csv-to-html --defaults s-exoression-for-default-values`
  `s-expression-for-default-values` is a double-quoted string with the same syntax as for the fields names(see above).
  * Fields which accept default values are:
    * `phone-number`, by default "n.a."
    * `price-range`, by default "Free/Gratuit" for bilingual calendar, "Free" for english and "Gratuit" for french
    
  
  
  
