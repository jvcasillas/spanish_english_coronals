filePath$ = "/Users/simonet/Desktop/12_0250/segmented"

Create Strings as file list... dirFiles 'filePath$'/*.Sound
select Strings dirFiles
clearinfo
numberOfFiles = Get number of strings

printline token,subject,word,group,language,cons,vot,cog,sd,skew,kurt,cm,mint

for i to numberOfFiles
	select Strings dirFiles
	fileName$ = Get string... i
	prefix$ = fileName$ - ".Sound"
	Read from file... 'filePath$'/'prefix$'.Sound
	Read from file... 'filePath$'/'prefix$'.TextGrid
	subject$ = mid$(prefix$,5,8)
	group$ = Get label of interval... 5 1
	language$ = Get label of interval... 4 1
	word$ = Get label of interval... 3 1
	cons$ = left$(word$,1)
	voicing = Get time of point... 2 1
	release = Get time of point... 1 1
	vot = (voicing - release) * 1000
	window = release + 0.02

	if language$ = "english"
		language$ = "English"
	elsif language$ = "spanish"
		language$ = "Spanish"
	endif

	if group$ = "BIL"
		group$ = "bilingual"
	elsif group$ = "NSP"
		group$ = "Spanish"
	elsif group$ = "NEN"
		group$ = "English"
	endif

	if word$ = "dada"
		word$ = "daba"
	elsif word$ = "daga "
		word$ = "daga"
	elsif word$ = "daltonian "
		word$ = "daltonian"
	elsif word$ = "daltonico "
		word$ = "daltonico"
	elsif word$ = "damnation "
		word$ = "damnation"
	elsif word$ = "damnaton"
		word$ = "damnation"
	elsif word$ = "dancet"
		word$ = "dancette"
	elsif word$ = "danceur"
		word$ = "danseur"
	elsif word$ = "dancing "
		word$ = "dancing"
	elsif word$ = "dancinge"
		word$ = "dancing"
	elsif word$ = "tabard "
		word$ = "tabard"
	elsif word$ = "taberna "
		word$ = "taberna"
	elsif word$ = "tacit "
		word$ = "tacit"
	elsif word$ = "tactics "
		word$ = "tactics"
	elsif word$ = "tambourine "
		word$ = "tambourine"
	elsif word$ = "tamourine"
		word$ = "tambourine"
	elsif word$ = "tapioca"
		word$ = "tapioca"
	elsif word$ = "tatooing"
		word$ = "tattooing"
	elsif word$ = "tattoing"
		word$ = "tattooing"	
	endif

	stress$ = "NA"

	if word$ = "daba"
		stress$ = "s"
	elsif word$ = "daga"
		stress$ = "s"
	elsif word$ = "dama"
		stress$ = "s"
	elsif word$ = "dano"
		stress$ = "s"
	elsif word$ = "danza"
		stress$ = "s"
	elsif word$ = "daltonico"
		stress$ = "u"
	elsif word$ = "danar"
		stress$ = "u"
	elsif word$ = "danesa"
		stress$ = "u"
	elsif word$ = "danino"
		stress$ = "u"
	elsif word$ = "danzar"
		stress$ = "u"
	elsif word$ = "tabla"
		stress$ = "s"
	elsif word$ = "tactil"
		stress$ = "s"
	elsif word$ = "tanque"
		stress$ = "s"
	elsif word$ = "tanto"
		stress$ = "s"
	elsif word$ = "taza"
		stress$ = "s"
	elsif word$ = "tabaco"
		stress$ = "u"
	elsif word$ = "taberna"
		stress$ = "u"
	elsif word$ = "tamano"
		stress$ = "u"
	elsif word$ = "tambien"
		stress$ = "u"
	elsif word$ = "tampoco"
		stress$ = "u"
	elsif word$ = "dagger"
		stress$ = "s"
	elsif word$ = "damage"
		stress$ = "s"
	elsif word$ = "damper"
		stress$ = "s"
	elsif word$ = "dancing"
		stress$ = "s"
	elsif word$ = "dazzle"
		stress$ = "s"
	elsif word$ = "dakota"
		stress$ = "u"
	elsif word$ = "daltonian"
		stress$ = "u"
	elsif word$ = "damnation"
		stress$ = "u"
	elsif word$ = "dancette"
		stress$ = "u"
	elsif word$ = "danielle"
		stress$ = "u"
	elsif word$ = "tabloid"
		stress$ = "s"
	elsif word$ = "tackle"
		stress$ = "s"
	elsif word$ = "tactics"
		stress$ = "s"
	elsif word$ = "tanker"
		stress$ = "s"
	elsif word$ = "tantrum"
		stress$ = "s"
	elsif word$ = "taboo"
		stress$ = "u"
	elsif word$ = "tambourine"
		stress$ = "u"
	elsif word$ = "tapioca"
		stress$ = "u"
	elsif word$ = "tattoo"
		stress$ = "u"
	elsif word$ = "tattooing"
		stress$ = "u"
	endif

	if stress$ = "s"
		stress$ = "stressed"
	elsif stress$ = "u"
		stress$ = "unstressed"
	endif

	select Sound 'prefix$'
	Extract part... release window "Gaussian1" 1 "yes"
	nameSpectrum$ = prefix$ + "_part"
	select Sound 'nameSpectrum$'
	To Spectrum... yes
	select Spectrum 'nameSpectrum$'
	cog = Get centre of gravity... 2
	sd = Get standard deviation... 2
	skew = Get skewness... 2
	kurt = Get kurtosis... 2
	cm = Get central moment... 3 2

	select Sound 'prefix$'
	To Intensity... 100 0 yes
	mint = Get mean... release window dB

	printline 'prefix$','subject$','word$','group$','language$','cons$','stress$','vot:2','cog:2','sd:2','skew:2','kurt:2','cm:2','mint:4'

	select all
	minus Strings dirFiles
	Remove
endfor

select all
Remove
