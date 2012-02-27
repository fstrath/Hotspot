DESCRIPTION:

This R script is used to identify 96 well "hotspots" in the CDAT
laboratory at ARUP. If you don't know what that means you shouldn't be
reading this. Data format is .txt and output is in .pdf format.

AUTHORS:
F. Strathmann (script)
R. Davis (txt layout)
C. Clark (input)
H. Carlisle (input)
G. McMillin (input)

BEFORE YOU BEGIN:
1. You must have R installed on the computer executing the code and
the ggplot2 package. IT will need to install R. You can install the
ggplot2 package from within R.

You will also need to add R.exe to your
$PATH. This only needs to be done once unless the PATH gets reset.

 - typical path to R is: C:\Program Files\R\YOUR VERSION OF R\bin\i386

 - To add to your PATH
  a. Open Control Panel
  b. Click Performance and Maintenance
  c. Click System
  d. Click the Advanced tab
  e. Click the Environment Variables button
  f. In the top window, double click the Path variable
  g. In the Variable value field, scroll to the end of the text and
     type a semi-colon followed by the path to R
	NOTE: See above for a typical path but remember to change "YOUR VERSION OF R" to your actual
	version. SOmething like "R-2.13.1")
  h. Click OK
  i. Click OK

2. Prior to initial execution, you must run the install.bat file
   inside of the Production folder.
   This will create "Hotspot data" and "Hotspot figure" folders and shortcuts to these folders
   and the .R script on the server.

3. You MUST copy the data files you'd like analyzed into the "HOTSPOT data"
   folder before running the script.
   NOTE: Any files in the data folder will be analyzed.

4. Double click the "HOTSPOT" icon and follow the instructions.

6. Your graphs will be in the "HOTSPOT figures" folder with names corresponding
   to original file names.

NOTES: It is a bad idea to try and change .bat and .R files or any
files in the "files" folder unless you know what you're doing. None of
the authors assume responsibilty for your mischief. If you've
unintentionally modified a file, please contact the CDAT Supervisor to
obtain copies.

Last Updated: 11/23/2011 by FStrathmann
