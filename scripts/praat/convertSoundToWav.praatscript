filefrom$ = "/Users/simonetm/Desktop/12 0250 English/12 0250 English Spanish Stimuli/Sounds/"
fileto$ = "/Users/simonetm/Desktop/12 0250 English/12 0250 English Spanish Stimuli/SoundsWavs/"

Create Strings as file list... fileList 'filefrom$'*.Sound
numberOfFiles = Get number of strings

for i to numberOfFiles
	select Strings fileList
	words$ = Get string... i
	word$ = words$ - ".Sound"
	Read from file... 'filefrom$''word$'.Sound
	Save as WAV file... 'fileto$''word$'.wav
	Remove
endfor
