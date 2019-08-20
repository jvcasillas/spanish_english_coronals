Create Strings as file list... soundFiles /Users/simonet/Desktop/dt_EngSpan_2012/dt_EnglishSpanish_Stimuli/SoundsAXB/*.Sound
select Strings soundFiles
numberOfFiles = Get number of strings

for i to numberOfFiles
	select Strings soundFiles
	soundName$ = Get string... i
	Read from file... /Users/simonet/Desktop/dt_EngSpan_2012/dt_EnglishSpanish_Stimuli/SoundsAXB/'soundName$'
	Scale peak... 0.99
	Write to binary file... /Users/simonet/Desktop/dt_EngSpan_2012/dt_EnglishSpanish_Stimuli/SoundsAXB/'soundName$'
	Remove
endfor