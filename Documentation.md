Disclaimer : Most of what is written here must be taken with a grain of salt, as it is based on my current understanding, and thus, might not be accurate (at all). 
# The Divisions
Divisions in COBOL are a very important part of the language. You must use a few of them to organize the program.

### IDENTIFICATION DIVISION.
It's basically a header, where you can put all kind of meta information on the program, who's written it, when, the program name, the patches dates and content (Yeah git wasn't a thing), and plenty more.
#### PROGRAM-ID. [name].
Here you can specify the program "name", I was told it cannot exceed 8 characters.
#### AUTHOR. [author_name].
Here you specify the author_name, I am not aware of any limitations.
#### DATE-WRITTEN. [date].
This field is used to write the date at which the program was written, it appears any format can be used, it doesn't even have to be an actual date, as I read that the date is treated as a comment
#### DATE-COMPILED. [date].
This field is used to write the date at which the program was compiled. Similarly to DATE-WRITTEN, it seems that the format can be anything.

### ENVIRONMENT DIVISION.
I heard you can put stuff in here to specify the files and basically any external data medium that you will need on the program. I don't know much about it tho as I have not used it, so this might be pretty much completely wrong.

### DATA DIVISION.
It is a least used to declare some variables in the WORKING-STORAGE SECTION, there are probably other uses.\
Learn about variable declaration later in the documentation

### PROCEDURE DIVISION.
This is where (I think) you put all your instructions, you can use the variables declared in the DATA DIVISION here.\
It is commonly finished with STOP RUN.

# Known keywords and commands
#### DISPLAY [text_to_display].
Used to display something to the terminal, I heard its main purpose is debugging.\
Variables (numeral and alphabetical) can be mixed here (I don't know whether it's a COBOL feature or a display feature), example :
##### DISPLAY "You are " AGE " years old.".
AGE being a variable, numeric or alphabetical.
#### ACCEPT [variable].
Used to read a value from stdin (standard input, it can be the user of a terminal), and stores the value in the specified variable.
#### MULTIPLY [var1] BY [var2] GIVING [var3].
Multiplies var1 by var2, and stores the result in var3.