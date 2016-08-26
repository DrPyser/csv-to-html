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
* TL;DR: 
  for bilingual calendar: `csv-to-html --bi -o path/to/output-file  path/to/events-csv path/to/locations-csv`
  for english calendar: `csv-to-html --en -o path/to/output-file  path/to/events-csv path/to/locations-csv` 
  for french calendar: `csv-to-html --fr -o path/to/output-file  path/to/events-csv path/to/locations-csv` 
  * If the one of the paths begins with a slash ("-"), you must double-quote the path(`"-path/to/file"` instead of `-path/to/file`)
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
  
* To print to standard output(which you can redirect to a file with '> *filename*' or pipe into another program with '| *program name* ...'),
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
  (see [this Link](http://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table) for specifications on valid date and time format syntax).
  The time format string must match the format of the times in the event's csv
  * Example of valid date format strings: 
    - "HH:mm:ss" (example of times matching this format: "14:12:00", "04:04:04")
    - "" (example of dates matching this format: "01/01/2016", "12/04/2005")
    - "yyyy-MM-dd" (example of dates matching this format: "2016-01-01", "2005-04-12")
* To change values of fields' names: `csv-to-html -f s-expression-for-fields-names` 
  where 's-expression-for-fields-names' is a double-quoted string with this syntax: `"((field . |field-name|) ...)"`
  * Each field specification is in a pair of parentheses, 
  * the field and the field's name are separated with spaces and a dot: "(field . |fieldname|)".__
  * The `|` characters before and after the field name are to escape the field name, 
    in case it contains special characters. 
  * Examples: `"((start-date . |_event_start_date|) (start-time . |_event_start_time|))"`
  * Alternatively, you can change the fields names of the csv so that they match the defaults of the program.
  * Also alternatively, you can change the default values of the program itself.
  
  
