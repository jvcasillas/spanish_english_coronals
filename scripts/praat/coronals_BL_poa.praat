# Coronales script ------------------------------------------------------------
#
# Created: 2019-08-20
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
filePath$ = "../../data/raw/BL_poa/"

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
	n_intervals = Get number of intervals: 1
	item$ = Get label of interval: 2, 1
	notes$ = Get label of interval: 3, 1

	if n_intervals = 1
		msg$ = "miss"
		f1start = 999
		f2start = 999
		f1mp = 999
		f2mp = 999
		vot = 999
		ri = 999
		cog = 999
		sd = 999
		sk = 999
		kt = 999

	else

    	burstInterval$ = Get label of interval: 1, 3
		if burstInterval$ = "b"
			b_int = 3
			v_int = 4
		elif burstInterval$ = "v"
			b_int = 2
			v_int = 3
		endif

			# VOT
			release = Get start time of interval: 1, b_int
			voicing = Get end time of interval: 1, b_int
			vot = (voicing - release) * 1000
			window = release + 0.006

			# Vowel info
			vowelStart = voicing
			vowelEnd  = Get end time of interval: 1, v_int
			durationV =  (vowelEnd - vowelStart) * 1000
			mp = vowelStart + ((vowelEnd - vowelStart) * 0.50)

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
			riV = Get value at time: mp, "Cubic"
			ri = riV - riB
		
	


		msg$ = "hit"

	endif


	printline 'prefix$','item$','n_intervals','msg$','f1start:2','f2start:2','f1mp:2',
	...'f2mp:2','durationV:2','vot:2','ri:2','cog:2','sd:2','sk:2','kt:2','notes$'
	select all
	minus Strings dirFiles
	Remove
endfor


select all
Remove
