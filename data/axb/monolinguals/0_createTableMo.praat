###################
#  Append tables        #
#  Joseph V. Casillas #
#  11/17/2014            #
##################

#### This script will do the following:
####	- open .table files from a folder
####	- create new 'participant' column
####       - append filename to rows of participant column

#### Enter the path to where the files are kept

form Enter information
	comment Folders where files are kept:
	sentence dirFiles /Users/casillas/Desktop/coronals/axb/monolinguals
endform

# Prepare the loop
Create Strings as file list... allFiles 'dirFiles$'/*.Table
select Strings allFiles
numberOfFiles = Get number of strings

for i  to numberOfFiles
	select Strings allFiles
	fileName$ = Get string... i
	prefix$ = fileName$ - ".Table"
	Read from file... 'dirFiles$'/'fileName$'
	nameTable$ = selected$("Table")
	select Table 'nameTable$'
	n = Get number of rows
	Remove column: "subject"
	Append column: "participant"
	Append column: "group"
	for x from 1 to n
		Set string value: x, "participant", prefix$
		Set string value: x, "group", "mo"
	endfor
	Save as tab-separated file: "'dirFiles$'/'prefix$'.txt"
endfor


