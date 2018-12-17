###########################################################################
###########################################################################
## Make VOT continuum wizard
##
## Matthew B. Winn 
##
## Version 10
## August 2014
##
## Three basic strategies:
##   1) Control VOT and F0 independently: say "NO" to covary
##   2) Control VOT, have a constant level of F0: 
##        say "NO" to covary and enter only 1 step of F0
##        and type in that constant F0 level as the "min" F0
##   3) Co-vary VOT and F0 by having a different F0 for each step of VOT
##        (This sounds most natural, and is intended for when
##         you aren;t interested in cue weighting, but are interested
##         simply in the perception of voicing)
##      
##  Control the time range over which the F0 changes 
##    from the continuum level to the original contour
##     using the 'f0timerangeMS' value. 50-100 is a reasonable range. 
##
## Control proportional VOT vowel cutback
##    - Normally long VOT values mean that the aspiration period cuts into the vowel
##       by making the vowel voiceless during the VOT.
##    - At default, this is a 1-to-1 timing tradeoff.
##    - If you want to cut into the vowel, but by an amount different than
##       the length fo the VOT, you can choose to do that 
##       by scrolling all the way to the bottom of the script
##       and changing the 'votCutbackRatio' variable. 
##       I don't recommend it. 
##
## Extracontrol:
##    - Control duration of F0 perturbation into the vowel 
##       and check fine-grained details of the pitch tracking
##       by checking the 'extracontrol' box in the startup window
##    - A good option if the pitch in your final stimuli don't seem to be correct,
##        because you can see whether the pitch was correctly tracked to start with.
##       - usually it means that you have incorrect pitch analysis settings 
##          (min & max pitch range), but you can override this
##           if you are especially skilled in the Manipulation window)
##
## Options to shape VOT and vowel amplitude contours at segment boundary
##     This is highly recommended. Otherwise, medium-lag sounds will sound goofy
##      because there isn't a neat envelope transition from the burst
##      to the vowel (this is especially noticable for /t/ sounds)
## 
## 
## You can prepare a pre-made aspiration sound
##    - If you want to modify it in strange ways before making the continuum
##    - ...or if you want to ensure that multiple continua
##     have the same exact VOT segments. 
##
## 'mainDir' names the sub-folder that will be created in the parent directory
##   (the parent directory should be a folder that already exists on your computer)
##
## 'basename' is a constant string prefix for each sound
##
## Note 1: this script is a mixture of different styles of Praat coding. Sorry for any confusion.
## Note 2: The saving functions probably wont work with Mac computers because of the / - \ switch in directories
##      ... so if you're on a mac, simply don't check the 'save' option;
##      ... you can batch-save the files later if you want. 
## Note 3: this script does not support the inclusion of pre-voicing. 
##
###########################################################################
###########################################################################


form Enter settings for F0 x VOT continuum 

	comment Enter VOT settings - number of steps, high (voiceless) & low (voiced) VOT endpoint values
		natural left_votsteps 8
		real right_votMax 85
		real right_votMin 10

	comment Enter number of F0 steps, and the max & min F0 values (if only one step, enter value in "min")
		natural left_f0steps 5
		real right_min_F0onset 90
		real right_max_F0onset 140

	comment Perfect covariance of VOT and F0 (non-orthogonal variation; "voicing" continuum) ?
	optionmenu covary: 1
	        option No - control them separately
        	option Yes - make a "voicing" continuum by tying F0 to VOT
        	
        comment Enter the time range (in ms) for F0 manipulation starting from vowel onset
		real f0timerangeMS 75

	comment Save files?
		boolean save 0
		
	comment Enter directory path for the new files (a folder that already exists)
	comment (Ignore if not saving files)
		sentence parentDir C:\Users\Matt\Desktop\Speech_Continua
		
	comment Enter basic names for the files and folder
		sentence left_mainDir VOT_GK
		sentence right_basename GK
		
     optionmenu Preselected_Burst_and_Aspiration: 1
        option Select VOT aspiration from original words
        option Use a pre-made VOT aspiration segment

	comment Use extra control in the manipulation? (advanced only)
		boolean extracontrol 0

endform

#
#
##
###
#####
########
#############
#####################
##################################
####################################################### 
## ACTIONS
######################################################################

call setVariables

clearinfo

if save = 1
	call makeDirectories
endif

# select the starting objects
	pause Select the voiced-onset sound
	voiced$ = selected$ ("Sound")
	voiced = selected ("Sound")

	pause Select the voiceless-onset sound
   	voiceless$ = selected$ ("Sound")
   	voiceless = selected ("Sound")

# match sampling frequency to the highest of the two sounds
	call maxSamplerate "'voiced$'" 'voiceless$'

call selectVOTSegment
	# yields objects: votSound$ , votSound

call makeSilence extraVOTbuffer extraVOTbuffer 1

call getVowelOnset 'voiced$'
	# yields variable vowelstartpoint

f0startpoint = vowelstartpoint + (votEndBlendTime/2)

call createManipulationObject

call checkF0tracking

call printContinuumvalues

#call initialize_editor_geoms

#call simplify_editor 'voiced$'

call makeVOTDurationcontinuum

call check_VOT_steps

call make_buffer

call make_VOT_cutbacks
    # yields vowel_cutback_'thisVOTStep'

call make_continuum

print used 'votEndBlendTime' as the VOT end blend time 'newline$'  

#call restore_editor_geoms 'voiced$'

if save = 1
   ## save the continuum info numbers in your folder
	call saveInfoWindow "'parentDir$'\'mainDir$'" 'outputFileName$'

   ## save the original sounds
   	call saveWavFile 'voiced$' 'parentDir$'\'mainDir$'\Original_sounds
   	call saveWavFile 'voiceless$' 'parentDir$'\'mainDir$'\Original_sounds
   	call saveWavFile 'votSound$' 'parentDir$'\'mainDir$'\Original_sounds
   
   ## make a list of all the files that you made
	call makeFileList "'parentDir$'\'mainDir$'\Stimuli\" 'listName$'
endif

## finished :)	

#
#
##
###
#####
########
#############
#####################
##################################
####################################################### 
## PROCEDURES
#######################################################
##################################
#####################
#############
########
#####
###
##
#
#


procedure make_buffer
   Create Sound from formula: "buffer", 1, 0, 1, samplerate, "0"
endproc

procedure make_VOT_cutbacks
  for thisVOTStep from 1 to votsteps
     adj_vot = step_VOT_'thisVOTStep' - step_VOT_1
     cutback_dur_'thisVOTStep' = adj_vot * votCutbackRatio
     if cutback_dur_'thisVOTStep' > max_cutback_dur
          cutback_dur_'thisVOTStep' = max_cutback_dur
     endif
     select voiced
     .dur = Get total duration
     Extract part: vowelstartpoint+cutback_dur_'thisVOTStep'-(votEndBlendTime/2), .dur, "rectangular", 1, "no"
     Rename... vowel_cutback_'thisVOTStep'
  endfor
endproc


procedure check_VOT_steps
	if extracontrol = 1
	   pause Check vot continuum steps to make sure they all seem reasonable
	endif
endproc

procedure make_continuum
    for thisVOTStep from 1 to votsteps
       selectObject: "Sound buffer"
       plusObject: "Sound vowel_cutback_'thisVOTStep'"
       Concatenate
       Rename: "temp_cutback_'thisVOTStep'"
       To Manipulation: 0.01, minpitch, maxpitch
       Extract pitch tier
    
    	   if covary = 0
    	      for thisF0step from 1 to f0steps
    	      newF0 = step_F0_'thisF0step'
    	   elsif covary = 1
    	      newF0 = step_F0_'thisVOTStep'
    	      thisF0step = 'thisVOTStep'
    	   endif
    	   
    	   select PitchTier temp_cutback_'thisVOTStep'
    	   Remove points between: 0, 1 + f0duration
    	   # add pitch point 10 ms into the vowel 
    	   Add point: 1.01, newF0
    	   selectObject: "Manipulation temp_cutback_'thisVOTStep'"
    	   plusObject: "PitchTier temp_cutback_'thisVOTStep'"
    	   Replace pitch tier
    	   selectObject: "Manipulation temp_cutback_'thisVOTStep'"
    	   Get resynthesis (overlap-add)
    	   Rename... junk
    	   .dur = Get total duration
    	   Extract part: 1, .dur, "rectangular", 1, "no"
    	   Rename: "vowel_cutback_'thisVOTStep'_f0_'thisF0step'"
    	   if shapeVowelAmplitude = 1
    	      ramp_duration = cutback_dur_'thisVOTStep'/2
    	      call halfFadeIn "vowel_cutback_'thisVOTStep'_f0_'thisF0step'" ramp_duration
	   endif
    	   
    	   
    	   select Sound junk
    	   Remove
    	   
    	   
    	   # put the sound components together
    	   # first establish formatted continuum step numbers
    	       call variableNameZeroAdjust
    	   
    	   # concatenate aspration and vowel
    	      select Sound VOT_'thisVOTStep'
    	      plus Sound vowel_cutback_'thisVOTStep'_f0_'thisF0step'
    	      Concatenate with overlap... votEndBlendTime
    	   
    	   # assign the name based on the formatted numbers
       	      Rename... 'basename$''f0Prefix$''thisF0step$''votPrefix$''thisVOTStep$'
    	   
    	   # save the file, if desired
    	      if save = 1
    	        call saveWavFile 'basename$''f0Prefix$''thisF0step$''votPrefix$''thisVOTStep$' 'parentDir$'\'mainDir$'\Stimuli
    	      endif
    	   
    	   # cleanup
    	      selectObject: "Sound vowel_cutback_'thisVOTStep'_f0_'thisF0step'"
    	      Remove
    	
    	# if you've been creating orthogonal VOT*F0 steps, end the F0 loop now
    		if covary = 0
    		      endfor
    		endif
    	
    	# cleanup for each step of the VOT continuum
      	   selectObject: "Manipulation temp_cutback_'thisVOTStep'"
    	   plusObject: "PitchTier temp_cutback_'thisVOTStep'"
    	   plusObject: "Sound temp_cutback_'thisVOTStep'"
    	   plusObject: "Sound vowel_cutback_'thisVOTStep'"
    	   Remove
    endfor
    
    select Sound buffer
    plus Manipulation 'voiced$'
    if extraVOTbuffer > 0
       plus Sound extraVOTbuffer
    endif
    Remove
endproc

procedure checkF0tracking
	temp.landmark = f0startpoint
	select Sound 'voiced$'
	To Pitch... 0 minpitch maxpitch
	f0_onset = Get value at time... f0startpoint Hertz Linear

	# scroll along the sound until you find an undefined point
	while f0_onset = undefined
	   f0startpoint = f0startpoint + 0.001
	   f0_onset = Get value at time... f0startpoint Hertz Linear
	endwhile

	# keep track fo the amount of adjusting that you did
	while.adjust = f0startpoint - temp.landmark

	# if the pitch can't be tracked within 25 ms of the user-selected landmark,
	# the user must define the pitch pulse tracking 
	# in the manipulation object
	if while.adjust > 0.025
		Edit
		editor Manipulation 'voiced$'
		beginPause ("Ensure all blue pulses are marked.")
		comment ("(ctrl+p to add pulses where there are missing blue lines)")
		comment ("")
		#comment ("If you don't know what to do,")
		#comment ("then consider working with a different sound recording")
		#comment ("that has better pitch tracking.")
		endPause ("Okay, I'm done",1)
		Close
		endeditor

	endif
	select Pitch 'voiced$'
	Remove
endproc

procedure printContinuumvalues
   # print VOT continuum values
	print VOT steps 'newline$'
   call makeContinuum3dec votsteps votMin votMax VOT_ 1
	print 'newline$'
	# yields variable levels with the name step_VOT_'n'
	
   # print F0 continuum values
	print F0 steps 'newline$'

   if f0steps >1
	call makeContinuum0dec f0steps min_F0onset max_F0onset F0_ 1
	print 'newline$'
	# yields variable levels with the name step_F0_'n'
   else
	step_F0_1 = min_F0onset
	print F0 onset:'tab$''min_F0onset:0' 'newline$'
   endif
endproc

procedure saveWavFile .name$ .directory$
	select Sound '.name$'
	Save as WAV file... '.directory$'\'.name$'.wav
endproc	

procedure makeFileList .soundDir$ listName$
	Create Strings as file list... 'listName$' '.soundDir$'
	Save as raw text file... 'parentDir$'\'mainDir$'\'listName$'.txt
	select Strings 'listName$'
	Remove
endproc	

procedure makeSilence .name$ .duration .numchannels
	Create Sound from formula... '.name$' '.numchannels' 0 '.duration' 'samplerate' 0
endproc

procedure saveInfoWindow outputDirectory$ outputFileName$
	filedelete 'outputDirectory$'\'outputFileName$'.txt
	fappendinfo 'outputDirectory$'\'outputFileName$'.txt
endproc


procedure lengthen2 .sound .required_duration .newname$
  # "Lengthen" by simply taking the last third
  # and appending it to the end of the original segment
  # this is ugly, but sounds better in the case of working with aspiration segments,
  # because the 'Lengthen' procedure produces spurious amplitude modulations. 

    # first check to see if it's feasible
	select .sound
	.name$ = selected$("Sound")
	.orig_duration = Get total duration
	.extra_needed = .required_duration - .orig_duration
	.multiplier = .required_duration/.orig_duration

   if .multiplier > 2.4
     # not feasible
	print required duration is '.required_duration:3' and your total sound duration is '.orig_duration:3' :( 'newline$'
	print lengthening your segment by the appropriate amount ('.multiplier:3') will cause too much distortion. 
	exit Sorry, the chosen sound is not long enough to support the VOT continuum you want :(      See Info Window for details. 
	
   elsif .extra_needed < (.orig_duration*0.4)
     # Simply append the last 0.4 (or less) to the original sound)
     print appending the last '.extra_needed:3' to the end of '.name$''newline$'
    	select .sound
    	Extract part: (.orig_duration-.extra_needed)-0.03, .orig_duration, "rectangular", 1, "no"
    	Rename... temp_offset
    	
    	select .sound
    	plus Sound temp_offset
    	Concatenate with overlap... 0.03
    	Rename... junk
    	Extract part: 0, .required_duration, "rectangular", 1, "no"
    	Rename... '.newname$'
    	.new_sound = selected("Sound")
    	
    	select Sound junk
    	plus Sound temp_offset
    	Remove
   elsif .extra_needed > (.orig_duration*0.4)
     print using regular Lengthen procedure 'newline$'
     print using multiplier of '.multiplier:3''newline$'
	select .sound
	Lengthen (overlap-add)... 100 300 .multiplier
    	Rename... '.newname$'
    	.new_sound = selected("Sound")
   endif
    
  select .new_sound
    
	# VOT segment has been lengthened to accomodate your continuum	   
endproc


procedure createManipulationObject
	select Sound 'voiced$'
	To Manipulation... 0.01 minpitch maxpitch
endproc

procedure getVowelOnset .sound$
	select Sound '.sound$'
	Edit
	editor Sound '.sound$'
	   # plax
	   Select... (0.182) (0.182)
	   pause click on the vowel onset
		Move cursor to nearest zero crossing
		vowelstartpoint = Get cursor
	   Close
	endeditor
endproc



procedure maxSamplerate .sound1$ .sound2$
  # if two sounds have different sampling rates,
  # up-sample the lower-sampled one. 
	select Sound '.sound1$'
	.samplerate1 = Get sampling frequency
	numchannels = Get number of channels
	
	select Sound '.sound2$'
	.samplerate2 = Get sampling frequency
	
	if .samplerate1 > .samplerate2
		samplerate = .samplerate1
		select Sound '.sound2$'
		Resample... 'samplerate' 50
		select Sound '.sound2$'
		Remove
		select Sound '.sound2$'_'samplerate'
		Rename... '.sound2$'
	
	elsif .samplerate2 > .samplerate1
		samplerate = .samplerate2
		select Sound '.sound1$'
		Resample... 'samplerate' 50
		select Sound '.sound1$'
		Remove
		select Sound '.sound1$'_'samplerate'
		Rename... '.sound1$'
	else
		samplerate = .samplerate1
	endif
endproc


procedure selectVOTSegment
   if preselected = 1
	pause Click on the sound to be used for the VOT burst & aspiration
	votSound$ = selected$ ("Sound")
	votSound  = selected ("Sound")
   elsif preselected = 0
        call select_VOT_from_original voiceless vot_segment
        votSound$ = selected$ ("Sound")
	votSound  = selected ("Sound")
   endif

	rawAspDuration = Get total duration
	# if the length of the aspiration segment doesn't support the entire VOT continuum,
		# it is lengthened and re-cross-faded; extensive lengthening (3x) forbidden. 
		requiredDuration = votMax+votEndBlendTime
	if rawAspDuration < requiredDuration
		#call lengthenVOT
		call lengthen2 votSound requiredDuration lengthened_VOT
		
		select Sound lengthened_VOT
		votSound$ = selected$ ("Sound")
		votSound  = selected ("Sound")
	endif
   endproc

procedure select_VOT_from_original .sound .newname$
    # you are selecting the aspiration segment from among the original sounds 
	    ## The aspiration segment comes from the voiceless-onset sound
		aspirationSound$ = "'voiceless$'"
		select '.sound'
		.name$ = selected$("Sound")
		Edit
		editor Sound '.name$'
		# plax
		Select... 0.144 0.144
		   pause Click on the START of the burst in the voiceless consonant
			Move cursor to nearest zero crossing
			aspiration_start = Get cursor
		# plax
		Select... 0.296 0.296
		   pause Click on the END of the aspiration in the voiceless consonant
			Move cursor to nearest zero crossing
			aspiration_end = Get cursor
			Close
			endeditor

		   ## Extract the aspiration 
		select Sound 'aspirationSound$'
		Extract part: aspiration_start, aspiration_end, "rectangular", 1, "no"	
		Rename... '.newname$'
endproc


procedure makeContinuum .steps .low .high .prefix$ .printvalues .decimals
	for thisStep from 1 to .steps

	temp = (('thisStep'-1)*('.high'-'.low')/('.steps'-1))+'.low'

	step_'.prefix$''thisStep' = temp
	.value = step_'.prefix$''thisStep'
	if .printvalues = 1
	print '.prefix$''thisStep''tab$''.value' 'newline$'
	endif

	endfor
endproc

procedure makeContinuum0dec .steps .low .high .prefix$ .printvalues
	for thisStep from 1 to .steps

	temp = (('thisStep'-1)*('.high'-'.low')/('.steps'-1))+'.low'

	step_'.prefix$''thisStep' = temp
	.value = step_'.prefix$''thisStep'
	if .printvalues = 1
	print '.prefix$''thisStep''tab$''.value:0' 'newline$'
	endif

	endfor
endproc

procedure makeContinuum3dec .steps .low .high .prefix$ .printvalues
	for thisStep from 1 to .steps

	temp = (('thisStep'-1)*('.high'-'.low')/('.steps'-1))+'.low'

	step_'.prefix$''thisStep' = temp
	.value = step_'.prefix$''thisStep'
	if .printvalues = 1
	print '.prefix$''thisStep''tab$''.value:3' 'newline$'
	endif

	endfor
endproc

procedure makeVOTDurationcontinuum
    for thisStep from 1 to votsteps
	# sets VOT duration, plus extra blend time for cross-fading
	   votDurationThisStep = step_VOT_'thisStep' + votEndBlendTime

	call shortenVOT votSound 'thisStep' 'votDurationThisStep'	
    endfor
endproc

procedure variableNameZeroAdjust
    ## re-align filename digits 
    #if there is a double-digit-member VOT continuum
    
    # first declare named digits
	thisVOTStep$ = "'thisVOTStep'"
	thisF0step$ = "'thisF0step'"

    # Additionally ... 
	# if there's only one F0 step,
   	# then omit the F0 prefix name altogether
	   if f0steps <2
	   # get rid of the F0 prefix if there is only one F0 step
		tempf0Prefix$ = ""
		thisF0Step$ = ""
	endif
   if format_fixed_digits = 1
	# if there are more than 9 steps,
	if votsteps > 9
		# if it's  single-digit index,
		if 'thisVOTStep' < 10
			# add a leading 0 to the index name	
			thisVOTStep$ = "0'thisVOTStep'"
		endif
	endif

	# do the same for the F0 steps
	if f0steps > 9
		if 'thisF0step' < 10
			thisF0Step$ = "0'thisF0step'"
		endif
	endif
   endif
	
endproc

procedure makeDirectories
   # makes new directories - one as the main directory and one for the stimuli
	system mkdir 'parentDir$'\'mainDir$'
	system mkdir 'parentDir$'\'mainDir$'\Stimuli
	system mkdir 'parentDir$'\'mainDir$'\Original_sounds
	
	pause directories made in 'parentDir$'\'mainDir$'
endproc

procedure shortenVOT .sound .number .newduration
	select '.sound'
	.name$ = selected$("Sound")
	extracted = Extract part... 0 '.newduration' rectangular 1 no
	
	if extraVOTbuffer > 0
	   select Sound extraVOTbuffer
	   plus extracted
	   Concatenate
	   Rename... VOT_'.number'
	else
	   select extracted
	   Copy... VOT_'.number'
	endif
	
	select extracted
	Remove
endproc

procedure halfFadeIn .name$ .absoluteFadeInDuration
   ## implements a HALF-cosine ramp to fade the segment to half its original level at onset
	select Sound '.name$'
	.start = Get start time
    Formula... if x<('.absoluteFadeInDuration')  
	...then self * (1+cos((x-('.absoluteFadeInDuration' - '.start'))/'.absoluteFadeInDuration' * pi/2))/2  
	...else self endif  
endproc


procedure setVariables
	#####################################################
	##convert variable names from the form

	# tells whether the user already has pre-selected burst & aspiration files
		preselected = 'preselected_Burst_and_Aspiration' - 1
	# tells whether there should be silent buffers to push all the vowels to alignment
		#equalizeduration = 'alignment_of_Vowel_Onset' - 1

		votsteps = left_votsteps
		votMax = right_votMax/1000
		votMin = right_votMin/1000


		f0steps = left_f0steps
		min_F0onset = right_min_F0onset
		max_F0onset = right_max_F0onset
		#f0above = right_f0above
		#f0below = right_f0below

	# convert F0 time range from milliseconds to seconds 
		f0duration = (f0timerangeMS/1000)

	# if you want to co-vary the F0 along with the VOT,
	# then the F0 continuum needs to have the same number of steps as the
	# VOT continnuum
	covary = covary - 1
	if covary = 1
		f0steps = votsteps
	endif

	# main naime prefix for the objects
		basename$ = "'right_basename$'"
		mainDir$ = "'left_mainDir$'"

	############################################
	# Set some hard-coded variables
	# basic booleans
		yes = 1
		no = 0
		
	# blank
		blanksuffix$ = ""

	# intra-filename continuum factor prefixes
		votPrefix$ = "_VOT_"	
		f0Prefix$ = "_F0_"
	
	# Ensure that single-digit numbers have a leading 0 
	# to align them in character space
		format_fixed_digits = 1

	# sets output (Continuum info) file name using the basename from the folder
		outputFileName$ = "'basename$'_Continuum_Info"

	# sets name for the file list using the basename from the folder
		listName$ = "'basename$'_file_list"

	## pitch analysis settings - change if you have a super-low or super-high frequency talker
		minpitch = 65
		maxpitch = 250

	## the original (cutback) vowel has the following pseudo-risetime applied to it
		## over this duration, a half-cosine fade will be applied, so that it 
		##  goes from *half* amplitude to *full* amplitude
		## this makes the voiceless items smoother by adding an amplitude pinch at the segment boundary
		ramponset = 0.04

	# don't re-scale the amplitude of the envelope-multiplied resynthesized vowel (as this can unnaturally strengthen it)
		scaleChoice$ = "no"

	# Sets an absolute buffer duration for silence before the sound file
		extraVOTbuffer = 0.05
	
	# shapes the vowel onset to half-attenuate it at onset (1=yes / 0=no)
		shapeVowelAmplitude = 1

	# sets a window across which the VOT will be extended as it fades into the vowel
		## for example, 0.01 means that the aspiration will go 10ms past the "VOT" value 
		## and that extension will blend into the vowel onset to create a smooth transition
		votEndBlendTime = 0.007
		
	# sets a ratio of vot duration to vowel cutback duration. If this value is 2, then for every 2ms of vot, 1ms of vowel is cutback. 
		# a value of 1 means that for each ms of vot, there is a corresponding ms of vowel cutback. 
		# all this will happen after the burst 
		votCutbackRatio = 0.65
		
		# maximum duration of vowel to cut back:
		max_cutback_dur = 0.07
endproc