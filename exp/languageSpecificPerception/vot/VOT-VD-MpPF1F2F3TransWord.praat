########
#
# VOT, vowel duration, midpoints (pitch F1, F2, F3), word, and the 1st seven formant transitions (F1 F2 & F3)
#
#
#If your textGrids are setup correctly and quantum flux is not materializing unwanted bogons into your CPU, 
#this script should, for all intents and purposes give you (1) the duration between two intervals on tier 1, 
#for measuring VOT. Based on the intervals in tier two, it should also register (2) the mid-point
#values of the pitch, F1, F2, F3 frequencies and (3) the duration of vowel. Based on the same intervals in tier two, 
#the script should also (4) write the frequencies of the first seven formant values (F1, F2 & F3) in order to 
#analyze the transition patterns following VOT. This is achieved by reading the value of the first formant point 
#located at the initial interval then subsequently shifting to the following point based on the time step duration 
#between the formants. Any information placed on the third tier e.g., 'word' or speaker name will (5) also be 
#registered to the output file (named dats.txt). 
#
#As a bonus, you get to select the gender of the speaker which will change the view range frequency (Hz) to 5000 for
#men and 5500 for women. At the same time, by selecting gender, the pitch will be changed to 75-500 Hz for women and 
#to 50-300 Hz for men. You shouldn't need to have multiple files since this script will run through both a large segmented 
#files with multiple intervals or small individual files with a single interval on each tier. Any feedback is appreciated. 
#
#==============================================================================
#                              WAVE form        '''/\/\/\/\/\ '' /\/\/\
#==============================================================================
#                                                  .........  ...........  
#                                                   .      .   ..
#                            Spectrogram             ......      .....
#                                                    ......      .....
#                                                  ..      .. ...     ..
#==============================================================================
#  Tier one - VOT intervals                     |p|  
#==============================================================================
#  Tier two - Vowel intevals                      |a|
#==============================================================================
#  Tier three - name                            |       &&P[a]nga&&    |  
#==============================================================================
#
#Quick tip - marking up the 'word' in tier three with reference makers e.g., [] &&, makes for easy data formatting later on!  
#
#All the data can be found in an output file named 'Dats.txt'.
#
#Word | Speaker's Code | Gender | File Name | Stop Consonant | VOT Duration | Following Vowel | Pitch | F1 | F2 | F3 | Vowel Duration |F1_1|F1_2|F1_3|F1_4|F1_5|F1_6|F1_7|F2_1|F2_2|F2_3|F2_4|F2_5|F2_6|F2_7|F3_1|F3_2|F3_3|F3_4|F3_5|F3_6|F3_7
#
#Written by: Jesse Stewart - umste247@myumanitoba.ca
#
#Oh, I make no claims that this file will work on anything other than Windows 7.
#
#PS I'm not a programmer. Apologies for the lack of succinctness. 
#
########

####
#User interface
####
form Calculate durtion, F0, F1,F2, F3 in a textgrid
	comment Where do you keep your sound files and textGrids? (Win users - don't forget the final '\')
		text directory K:\Guarani\0i\Luz\test\
	comment Men or women? Type 'm' or 'f' only:
		sentence gender ('m' or 'f' only)
	comment Do you have a code you'd like to suffix to the file name?
		sentence code (Enter code here)
	comment Sound file extension:
		optionmenu file_type: 1
		option .wav
		option .aiff	
		option .mp3
		option .aifc
		option .au
		option .nist
		option .flac
		option .kay
		comment Tiers should contain the following information:
		comment Tier 1: VOT - Select the VOT bewteen intervals and add the phoneme here e.g., p t k
		comment Tier 2: Vowel - Select the portion of the vowel you want to analyze. Add the phoneme e.g., i e a o u
		comment Tier 3: Word - Add the word under analysis in this field.
		
		comment Written by: Jesse Stewart - umste247@myumanitoba.ca	
endform


####
#Output file
####
filedelete 'directory$'Dats.txt
titles$ = "Word" + tab$ + "Code" + tab$ + "Gender" + tab$ + "Filename" + tab$ + "Stop" + tab$ + "Duration" + tab$ + "Vowel" + tab$ + "F0_mid" + tab$ + "F1_mid" + tab$ + "F2_mid" + tab$ + "F3_mid" + tab$ + "Vowel_duration" + tab$ + "F1_1" + tab$ + "F1_2" + tab$ + "F1_3" + tab$ + "F1_4" + tab$ + "F1_5" + tab$ + "F1_6" + tab$ + "F1_7" + tab$ + "F2_1" + tab$ + "F2_2" + tab$ + "F2_3" + tab$ + "F2_4" + tab$ + "F2_5" + tab$ + "F2_6" + tab$ + "F2_7" + tab$ + "F3_1" + tab$ + "F3_2" + tab$ + "F3_3" + tab$ + "F3_4" + tab$ + "F3_5" + tab$ + "F3_6" + tab$ + "F3_7" + tab$ + newline$
titles$ > 'directory$'Dats.txt

####
#This section creates a 'strings' of the sound files in the assigned directory.
####
Create Strings as file list...  list 'directory$'*'file_type$'
number_files = Get number of strings

####
#This section sets up a 'for loop' which should analyze each file in the string list one time.
####

for x from 1 to number_files
	select Strings list
	filename$= Get string... 'x'
	Read from file... 'directory$''filename$'

####
#This section changes the view windown based on the gender selection
####
object_name$ = selected$ ("Sound")

	if gender$ = "f"
	do ("To Formant (burg)...", 0, 5, 5500, 0.025, 50)
	else
	do ("To Formant (burg)...", 0, 5, 5000, 0.025, 50)
	endif

####
#This section changes the pitch based on the gender selection
####
select Sound 'object_name$'

	if gender$ = "f"
	do ("To Pitch...", 0, 75, 500)
	else
	do ("To Pitch...", 0, 50, 300)
	endif


####
#Now to grab the corresponding textGrid
####
Read from file... 'directory$''object_name$'.TextGrid
select TextGrid 'object_name$'

	select TextGrid 'object_name$'
	number_of_intervals = Get number of intervals... 1 

	for y from 1 to number_of_intervals
		select TextGrid 'object_name$'
		interval_label$ = Get label of interval... 1 'y'
		if interval_label$ <> ""
			select TextGrid 'object_name$'

			interval_label3$ = Get label of interval... 3 'y'
				fileappend "'directory$'Dats.txt" 'newline$''interval_label3$''tab$'

				initialTime = Get starting point... 1 'y'
				endTime = Get end point... 1 'y'
				duration = (endTime - initialTime) * 1000
				fileappend "'directory$'Dats.txt" 'code$''tab$''gender$''tab$''object_name$''tab$''interval_label$''tab$''duration:1''tab$'
			
			interval_label2$ = Get label of interval... 2 'y'			
				initialTime2 = Get starting point... 2 'y'
				endTime2 = Get end point... 2 'y'
				duration2 = (endTime2 - initialTime2) * 1000
				midpoint = initialTime2 +((endTime - initialTime2)/2)
				
			select Formant 'object_name$'
				f_one = Get value at time... 1 'midpoint' Hertz Linear
				f_two = Get value at time... 2 'midpoint' Hertz Linear
				f_three = Get value at time... 3 'midpoint' Hertz Linear
			select Pitch 'object_name$'
				f_zero = Get value at time... 'midpoint' Hertz Linear
			
			select Formant 'object_name$'
				time_step = Get time step
				f1_second = initialTime2 + time_step
				f1_third = initialTime2 + (time_step * 2)
				f1_forth = initialTime2 + (time_step * 3)
				f1_fifth = initialTime2 + (time_step * 4)
				f1_sixth = initialTime2 + (time_step * 5)
				f1_seventh = initialTime2 + (time_step * 6)				
				f1_1 = Get value at time... 1 'initialTime2' Hertz Linear
				f1_2 = Get value at time... 1 'f1_second' Hertz Linear
				f1_3 = Get value at time... 1 'f1_third' Hertz Linear
				f1_4 = Get value at time... 1 'f1_forth' Hertz Linear
				f1_5 = Get value at time... 1 'f1_fifth' Hertz Linear
				f1_6 = Get value at time... 1 'f1_sixth' Hertz Linear
				f1_7 = Get value at time... 1 'f1_seventh' Hertz Linear

				f2_second = initialTime2 + time_step
				f2_third = initialTime2 + (time_step * 2)
				f2_forth = initialTime2 + (time_step * 3)
				f2_fifth = initialTime2 + (time_step * 4)
				f2_sixth = initialTime2 + (time_step * 5)
				f2_seventh = initialTime2 + (time_step * 6)				
				f2_1 = Get value at time... 2 'initialTime2' Hertz Linear
				f2_2 = Get value at time... 2 'f2_second' Hertz Linear
				f2_3 = Get value at time... 2 'f2_third' Hertz Linear
				f2_4 = Get value at time... 2 'f2_forth' Hertz Linear
				f2_5 = Get value at time... 2 'f2_fifth' Hertz Linear
				f2_6 = Get value at time... 2 'f2_sixth' Hertz Linear
				f2_7 = Get value at time... 2 'f2_seventh' Hertz Linear
				
				f3_second = initialTime2 + time_step
				f3_third = initialTime2 + (time_step * 2)
				f3_forth = initialTime2 + (time_step * 3)
				f3_fifth = initialTime2 + (time_step * 4)
				f3_sixth = initialTime2 + (time_step * 5)
				f3_seventh = initialTime2 + (time_step * 6)				
				f3_1 = Get value at time... 3 'initialTime2' Hertz Linear
				f3_2 = Get value at time... 3 'f3_second' Hertz Linear
				f3_3 = Get value at time... 3 'f3_third' Hertz Linear
				f3_4 = Get value at time... 3 'f3_forth' Hertz Linear
				f3_5 = Get value at time... 3 'f3_fifth' Hertz Linear
				f3_6 = Get value at time... 3 'f3_sixth' Hertz Linear
				f3_7 = Get value at time... 3 'f3_seventh' Hertz Linear

				fileappend "'directory$'Dats.txt" 'interval_label2$''tab$''f_zero:1$''tab$''f_one:1$''tab$''f_two:1$''tab$''f_three:1$''tab$''duration2:1''tab$''f1_1:1$''tab$''f1_2:1''tab$''f1_3:1''tab$''f1_4:1''tab$''f1_5:1''tab$''f1_6:1''tab$''f1_7:1''tab$''f2_1:1$''tab$''f2_2:1''tab$''f2_3:1''tab$''f2_4:1''tab$''f2_5:1''tab$''f2_6:1''tab$''f2_7:1''tab$''f3_1:1$''tab$''f3_2:1''tab$''f3_3:1''tab$''f3_4:1''tab$''f3_5:1''tab$''f3_6:1''tab$''f3_7:1'
		endif
	endfor

####
#This allows the script to select the next object in the Strings list
####
select all
     minus Strings list
     Remove
endfor
clearinfo
print That should be it! 'newline$'
print Send any inquires to: Jesse Stewart - umste247@myumanitoba.ca