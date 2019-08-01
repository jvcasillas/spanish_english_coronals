# Coronales script ------------------------------------------------------------
#
# Created: a long time ago
# Updated: 2019-08-01
#
# This script does the following: 
#
# 1. Calculate spectral moments based on a 6ms window after stop release
# 2. Calculate VOT
# 3. Calculate relative intensity (burst - midpoint of following vowel)
# 4. F1/F2 measurements at the beginning and midpoint of the vowel
#
# -----------------------------------------------------------------------------


# Set path to files
filePath$ = "../../data/segmentedNew/"

# Set up loop
Create Strings as file list... dirFiles 'filePath$'/*.wav
select Strings dirFiles
clearinfo
numberOfFiles = Get number of strings
clearinfor


# Begin loop
for i to numberOfFiles
	select Strings dirFiles
	fileName$ = Get string... i
	prefix$ = fileName$ - ".wav"
	Read from file... 'filePath$'/'prefix$'.wav
	Read from file... 'filePath$'/'prefix$'.TextGrid
	points = Get number of points... 1
	labels = Count labels: 4, "exclude"
	labID$ = Get label of interval: 4, 1

	if labels = 0
	
		# VOT
		if points = 1
		voicing = Get time of point... 2 1
		release = Get time of point... 1 1
		vot = (voicing - release) * 1000
		window = release + 0.006

		# Vowel info
		vowelStart = Get start point: 3, 2
		vowelEnd  = Get end point: 3, 3
		durationV =  vowelEnd - vowelStart
		mp = vowelStart + (durationV * 0.50)

		# Vowel formants
		select Sound 'prefix$'
		do ("To Formant (burg)...", 0, 5, 5500, 0.025, 50)
		f1H = do ("Get value at time...", 1, vowelStart, "Hertz", "Linear")
		f2H = do ("Get value at time...", 2, vowelStart, "Hertz", "Linear")
		f1Hmp = do ("Get value at time...", 1, mp, "Hertz", "Linear")
		f2Hmp = do ("Get value at time...", 2, mp, "Hertz", "Linear")
		# Convert to mel
		f1start = hertzToMel(f1H)
		f2start = hertzToMel(f2H)
		f1mp = hertzToMel(f1Hmp)
		f2mp = hertzToMel(f2Hmp)

		# Spectral moments and RI
		select Sound 'prefix$'
		Extract part... release window "Gaussian1" 1 "yes"
		nameSpectrum$ = prefix$ + "_part"
		select Sound 'nameSpectrum$'
		To Spectrum... yes
		select Spectrum 'nameSpectrum$'
		cog = Get centre of gravity... 2
		sd = Get standard deviation... 2
		sk = Get skewness... 2
		kt = Get kurtosis... 2

		select Sound 'prefix$'
		To Intensity... 100 0 yes
		riB = Get mean... release window dB
		riV = Get mean... vowelStart vowelEnd dB
		ri = riV - riB

	endif

	printline 'prefix$','f1start:2','f2start:2','f1mp:2','f2mp:2','vot:2','ri:2','cog:2','sd:2','sk:2','kt:2','labID$'
	select all
	minus Strings dirFiles
	Remove
	endif
endfor

select all
Remove
