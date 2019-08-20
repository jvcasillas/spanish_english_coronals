##########################
# Praat script to create textgrids
# Created by                     
# Joseph V. Casillas 12/08/2014  
###########################


#### Enter the path to where the files are kept

form Enter information
	comment Folders where files are kept:
	sentence dirFiles ../segmented/
	sentence newDir ../segmentedNew/
	positive number 1
endform


#### Prepare the loop
Create Strings as file list: "allFiles", dirFiles$ + "/*.Sound"
select Strings allFiles
numberOfFiles = Get number of strings
clearinfo
#### Begin loop

for i from number to numberOfFiles
	select Strings allFiles
	fileName$ = Get string... i
	prefix$ = fileName$ - ".Sound"
	Read from file... 'dirFiles$'/'fileName$'
	nameSound$ = selected$("Sound")
        To TextGrid: "release voice vowel note", "release voice"
	select TextGrid 'nameSound$'
        select Sound 'nameSound$'
	plus TextGrid 'nameSound$'
	Edit
	pause Continue?
	select Sound 'nameSound$'
	Write to WAV file... 'newDir$'/'nameSound$'.wav
	select TextGrid 'nameSound$'
	Write to binary file... 'newDir$'/'nameSound$'.TextGrid
	select all
	minus Strings allFiles
	Remove
	printline 'nameSound$'	'i'
endfor