##########################
# Praat script to select 25%     
# of segmented files (at random) 
# to check segemtation           
# Created by                     
# Joseph V. Casillas 12/08/2014  
###########################


#### Enter the path to where the files are kept

form Enter information
	comment Folders where files are kept:
	sentence dirFiles ../segmented/
	sentence dirNew ../segmented25/
endform


#### Prepare the loop
Create Strings as file list: "allFiles", dirFiles$ + "/*.Sound"
select Strings allFiles
Randomize
numberOfFiles = Get number of strings
writeInfoLine: numberOfFiles
twntyFivePerc = numberOfFiles * 0.25
writeInfoLine: round: twntyFivePerc

#### Begin loop

for i to twntyFivePerc
	select Strings allFiles
	fileName$ = Get string... i
	prefix$ = fileName$ - ".Sound"
	tgName$ = prefix$ + ".TextGrid"
	Read from file... 'dirFiles$'/'fileName$'
	nameSound$ = selected$("Sound")
	Read from file... 'dirFiles$'/'tgName$'
        #select TextGrid 'nameSound$'
        #Insert point tier: 2, "word"
        select Sound 'nameSound$'
	plus TextGrid 'nameSound$'
	Edit
	pause Continue?
	select Sound 'nameSound$'
	Write to WAV file... 'dirNew$'/'nameSound$'.wav
	select TextGrid 'nameSound$'
	Write to binary file... 'dirNew$'/'nameSound$'.TextGrid
	select all
	minus Strings allFiles
	Remove
	printline 'nameSound$'	'i'
endfor