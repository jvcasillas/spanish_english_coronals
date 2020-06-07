# Coronales script ------------------------------------------------------------
#
# Created: 2019-08-20
# Updated: 2020-06-06
#
# This script does the following: 
#
# 1. Calculate spectral moments based on a 6ms window after stop release
# 2. Calculate VOT
# 3. Calculate relative intensity (midpoint of vowel - burst)
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
		f1_start = 999
		f2_start = 999
		f1_cent = 999
		f2_cent = 999
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
			durationV =  (vowelEnd - vowelStart)
			mp = vowelStart + (durationV * 0.50)
			per20 = vowelStart + (durationV * 0.20)
			per35 = vowelStart + (durationV * 0.35)
			per50 = vowelStart + (durationV * 0.50)
			per65 = vowelStart + (durationV * 0.65)
			per80 = vowelStart + (durationV * 0.80)

			# Vowel formants
			select Sound 'prefix$'
			do ("To Formant (burg)...", 0, 5, 5500, 0.025, 50)
			f1_00 = do ("Get value at time...", 1, vowelStart, "Hertz", "Linear")
			f1_20 = do ("Get value at time...", 1, per20, "Hertz", "Linear")
			f1_35 = do ("Get value at time...", 1, per35, "Hertz", "Linear")
			f1_50 = do ("Get value at time...", 1, per50, "Hertz", "Linear")    
			f1_65 = do ("Get value at time...", 1, per65, "Hertz", "Linear")
			f1_80 = do ("Get value at time...", 1, per80, "Hertz", "Linear")
		
			f2_00 = do ("Get value at time...", 2, vowelStart, "Hertz", "Linear")
			f2_20 = do ("Get value at time...", 2, per20, "Hertz", "Linear")
			f2_35 = do ("Get value at time...", 2, per35, "Hertz", "Linear")
			f2_50 = do ("Get value at time...", 2, per50, "Hertz", "Linear")    
			f2_65 = do ("Get value at time...", 2, per65, "Hertz", "Linear")
			f2_80 = do ("Get value at time...", 2, per80, "Hertz", "Linear")

        	# Calculate spectral centroids
			f1_cent_hz = (f1_20 + f1_35 + f1_50 + f1_65 + f1_80)/5
			f2_cent_hz = (f2_20 + f2_35 + f2_50 + f2_65 + f2_80)/5

			# Convert to mel
			f1_start = hertzToMel(f1_00)
			f2_start = hertzToMel(f2_00)
			f1_cent = hertzToMel(f1_cent_hz)
			f2_cent = hertzToMel(f2_cent_hz)

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


	printline 'prefix$','item$','n_intervals','msg$','f1_start:2','f2_start:2','f1_cent:2',
	...'f2_cent:2','vot:2','ri:2','cog:2','sd:2','sk:2','kt:2','notes$'
	select all
	minus Strings dirFiles
	Remove
endfor


select all
Remove
