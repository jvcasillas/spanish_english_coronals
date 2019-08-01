filePath$ = "../segmentedNew/"
newDir$ = "/Users/casillas/Desktop/test1/"

Create Strings as file list... dirFiles 'filePath$'/*.wav
select Strings dirFiles
clearinfo
numberOfFiles = Get number of strings

printline soundFileCode

for i to numberOfFiles
	select Strings dirFiles
	fileName$ = Get string... i
	prefix$ = fileName$ - ".wav"
	Read from file... 'filePath$'/'prefix$'.TextGrid

	Shift times to: "start time", 0

	Write to binary file... 'newDir$'/'prefix$'.TextGrid

endfor