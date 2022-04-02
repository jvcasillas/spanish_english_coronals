Coronal stop production in bilinguals
================

# Introduction

# Background and motivation

# Method

## Participants

The data include 42 participants from 3 populations: monolingual English
speakers, monolingual Spanish speakers, and bilingual Spanish-English
speakers. All participants were females between the ages of 18 and 23.
The monolingual English speakers were recorded in English and the
monolingual Spanish speakers were recorded in Spanish. The
Spanish-English bilinguals were recorded in both of their languages.

**Monolingual English speakers.** The study includes 8 monolingual
English speakers. They were undergraduate students at the University of
Arizona, born and raised in the US Southwest. The English speakers were
functionally monolingual, though they reported having taken introductory
Spanish courses. They were not able to maintain a basic conversation in
Spanish. All of the participants in this group reported English as their
native language and verified not having been exposed to any other
languages while growing up.

**Monolingual Spanish speakers.** The monolingual Spanish group
comprised 8 speakers that were recruited from the *Universitat de les
Illes Balears* campus community and were born and raised on the island
of Majorca, Spain. They reported that, although they had studied some
English in Spain, they were not able to maintain a basic conversation in
this language. The participants of this group also speak Catalan.
Importantly, there are no reported differences in the phonetic
realization of voice timing between the Spanish and Catalan, nor are
there place difference between the coronal stops.

**Bilingual speakers.** The English-Spanish bilinguals (n = 26) came
from Southern Arizona and Northern Mexico. There are two samples from
this population. The coronal dataset includes 17 speakers and the
bilabial dataset includes 9 speakers. The Spanish-English bilinguals
were undergraduate students at the University of Arizona in Tucson,
Arizona. The bilinguals were brought up by Spanish-speaking families and
were schooled mostly in English. They reported using English and Spanish
daily, both in the classroom as well as with their friends and
relatives.

## Metrics

F1/F2, voice onset time, relative intensity, center of gravity, standard
deviation, skewness, kurtosis

## Procedure

Decide if we will present 3 separate experiments with 3 different
methods sections.

## Statistical analyses

All analyses were conducted in `R` (R Core Team, 2019, version 4.1.3).
We use Bayesian multilevel models fitted in `Stan` using `brms`
(Bürkner, 2017, 2018, version 2.16.3). Bayesian Data Analysis (BDA) is
an alternative to frequentist statistical analysis.[1] For all models,
the criterion was standardized, or converted to z-scores, in order to
facilitate comparability between metrics. Continuous predictors were
also standardized and categorical predictors were sum-to-zero coded.
Thus for all models the intercept represents the outcome variable at the
grand mean in standardized units. We used regularizing, weakly
informative priors in all models (specifics below) with 4,000 iterations
(2,000 warm-up) running on 16 processing cores. We quantify our
uncertainty regarding a given effect by reporting point estimates
derived from the posterior predictive distribution, including the 95%
Highest Density Credible Intervals (HDI). Additionally, we assume a
negligible effect size of ± 0.1 (Cohen, 1988, 2013; Kruschke, 2018) in
order to establish a Region of Practical Equivalence (ROPE), for which
we assess the proportion of the HDI that falls within this interval.
Finally, we report the Maximum Probability of Effect (MPE), or the
Probability of Direction, as the proportion of the posterior
distribution that is of the median’s sign. We assume there to be
compelling evidence for a given effect when the HDI of the posterior
distribution does not contain 0 nor fall within the ROPE by a reasonable
margin and the MPE is close to 1.

# Results

The results are divided into 3 subsections dealing with (1) monolingual
data, (2) bilingual data, and (3) POA data. Each subsection presents the
results of 6 metrics: VOT, relative intensity, center of gravity,
kurtosis, standard deviation, and skewness. We report only relevant
effects. Please see the supplementary materials (Appendices A-D) for
complete model summaries.

## Experiment 1: Monolinguals

We modeled VOT and the burst metrics as a function of language (English,
Spanish), phoneme (/d/, /t/), standardized F1 and F2, and item
repetitions. The model used regularizing, weakly informative priors
(Gelman, Simpson, & Betancourt, 2017; Vasishth et al., 2018).
Specifically, all parameters were assumed to be distributed as normal
with a standard deviation of 5, i.e.,
![Normal(\\mu = 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Normal%28%5Cmu%20%3D%200 "Normal(\mu = 0"),
![\\sigma = 5)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma%20%3D%205%29 "\sigma = 5)").
The standard deviation parameters for random effects and the model
residual error (sigma) were truncated to exclude negative values.
Figure @ref(fig:plot-monolinguals) plots VOT and the burst metrics as a
function of language (English, Spanish) and phoneme (/d/, /t/). For all
plots the y-axis represents the outcome variable in standardized units.
The x-axis indicates the language and voiced/voiceless pairs are
represented by color.

(ref:plot-monolinguals) VOT and burst metrics of coronal stops (/d/,
/t/) from monolingual speakers as a function of language (English,
Spanish). Transparent points represent raw data. Solid points indicate
posterior means ± 95% and 66% credible intervals.

![(ref:plot-monolinguals)](/Users/casillas/academia/research/in_progress/spanish_english_coronals/figs/mono_all_metrics.pdf)

**Voice-onset time.** English stops had higher VOT than Spanish stops (β
= 0.660, HDI = \[0.556, 0.763\], ROPE = 0, MPE = 1) and the voiceless
segments had higher VOT than the voiced segments (β = −0.617, HDI =
\[−0.697, −0.544\], ROPE = 0, MPE = 1). We also find moderate evidence
for an interaction between the two predictors (β = 0.108, HDI = \[0.033,
0.180\], ROPE = 0.406, MPE = 0.995). Specifically, the voicing
difference between Spanish coronals was slightly larger than that of the
English coronals. We compared the short-lag stops of each language,
English /d/ and Spanish /t/, and found no evidence that the segments
differed from each other (β = 0.085, HDI = \[−0.181, 0.335\], ROPE =
0.489, MPE = 0.745), as nearly half the HDI fell within the
predetermined region of practical equivalence. The VOT data is plotted
in the first panel of Figure @ref(fig:plot-monolinguals). The complete
model summary and the short-lag stop comparison are available in Table
@ref(tab:mono-table) and Figure @ref(fig:plot-monolinguals-d-t) of the
supplementary materials.

**Relative intensity.** The relative intensity data is plotted in the
top middle panel of Figure @ref(fig:plot-monolinguals). English and
Spanish stops differed little with regard to relative intensity. The
model provided no compelling evidence for a group effect (β = 0.026, HDI
= \[−0.171, 0.246\], ROPE = 0.666, MPE = 0.602), nor for phoneme (β =
−0.060, HDI = \[−0.192, 0.055\], ROPE = 0.742, MPE = 0.83) or F2 effects
(β = −0.080, HDI = \[−0.177, 0.015\], ROPE = 0.676, MPE = 0.948). The
height of the following vowel did appear to modulate relative intensity
of the burst to some degree (β = −0.172, HDI = \[−0.252, −0.085\], ROPE
= 0.026, MPE = 1), such that higher F1 values were associated with lower
relative intensity. That being said, approximately 2.6% of the HDI fell
within the region of practical equivalence. Finally, there was no
evidence supporting a language × phoneme interaction (β = 0.025, HDI =
\[−0.094, 0.157\], ROPE = 0.906, MPE = 0.657).

**Center of gravity.** The center of gravity (COG) data is plotted in
the first row, third panel of Figure @ref(fig:plot-monolinguals).
English stops had a higher COG than Spanish stops (β = 0.687, HDI =
\[0.555, 0.810\], ROPE = 0, MPE = 1), and, overall, voiceless segments
had a higher COG than the voiced segments (β = −0.298, HDI = \[−0.354,
−0.244\], ROPE = 0, MPE = 1), however, the two predictors interacted (β
= 0.214, HDI = \[0.157, 0.266\], ROPE = 0, MPE = 1). The interaction was
driven by a large COG difference between Spanish coronals (Spanish /d/
vs. Spanish /t/: β = −1.024, HDI = \[−1.184, −0.868\], ROPE = 0, MPE =
1) that was not present in the English coronals (English /d/ vs. English
/t/: β = −0.169, HDI = \[−0.327, −0.012\], ROPE = 0.172, MPE = 0.983)).
No other predictors had an effect on COG.

**Kurtosis.** The kurtosis data is plotted in the second row, first
column of Figure @ref(fig:plot-monolinguals). English stop bursts had
lower kurtosis with regard to Spanish stop bursts (β = −0.646, HDI =
\[−0.755, −0.542\], ROPE = 0, MPE = 1), and the voiced segments
presumably had a higher kurtosis than the voiceless segments (β = 0.290,
HDI = \[0.221, 0.361\], ROPE = 0, MPE = 1), though there was evidence of
a language × phoneme interaction (β = −0.263, HDI = \[−0.332, −0.195\],
ROPE = 0, MPE = 1). Specifically, kurtosis was higher in the voiced stop
bursts of Spanish (Spanish /d/ vs. Spanish /t/: β = 1.106, HDI =
\[0.905, 1.292\], ROPE = 0, MPE = 1), but there was no evidence of a
voicing difference in the English data (English /d/ vs. English /t/: β =
0.054, HDI = \[−0.149, 0.247\], ROPE = 0.659, MPE = 0.712). Neither F1
(β = −0.027, HDI = \[−0.067, 0.015\], ROPE = 1, MPE = 0.903) nor F2 (β =
0.001, HDI = \[−0.038, 0.043\], ROPE = 1, MPE = 0.509) had any influence
on kurtosis.

**Standard deviation.** With regard to standard deviation, we observe
the same pattern found in the COG data (See third panel, first row and
second panel, second row in Figure @ref(fig:plot-monolinguals)). That
is, there was a difference between English and Spanish (β = 0.490, HDI =
\[0.343, 0.648\], ROPE = 0, MPE = 1), as well as between phonemes (β =
−0.282, HDI = \[−0.355, −0.204\], ROPE = 0, MPE = 1), though, again,
there two predictors interacted (β = 0.210, HDI = \[0.141, 0.293\], ROPE
= 0, MPE = 1). Specifically, there was only a voicing difference for
Spanish (Spanish /d/ vs. Spanish /t/: β = −0.985, HDI = \[−1.193,
−0.772\], ROPE = 0, MPE = 1), where we see lower standard deviation
values in voiced stops. No such difference is observed between the
English coronals (English /d/ vs. English /t/: β = −0.143, HDI =
\[−0.365, 0.066\], ROPE = 0.335, MPE = 0.91). Height and frontedness of
the following vowel had no effect on standard deviation in the stop
burst (F1: β = −0.001, HDI = \[−0.051, 0.051\], ROPE = 1, MPE = 0.509;
F2: β = 0.007, HDI = \[−0.044, 0.061\], ROPE = 1, MPE = 0.595).

**Skewness.** The analysis of skewness of the stop burst showed a
similar pattern as the one observed in the analysis of kurtosis. If we
compare the first and third panels (second row) of Figure
@ref(fig:plot-monolinguals), we observe a language effect (β = −0.665,
HDI = \[−0.774, −0.561\], ROPE = 0, MPE = 1), such that Spanish stops
show higher skewness values, as well a voicing effect (β = 0.290, HDI =
\[0.222, 0.362\], ROPE = 0, MPE = 1), which is driven by a language ×
phoneme interaction (β = −0.221, HDI = \[−0.292, −0.155\], ROPE = 0, MPE
= 1). Specifically, the Spanish voiced coronal had higher skewness of
the burst than the voiceless counterpart (Spanish /d/ vs. Spanish /t/: β
= 1.023, HDI = \[0.827, 1.214\], ROPE = 0, MPE = 1), and this difference
is not present in the English data (English /d/ vs. English /t/: β =
0.139, HDI = \[−0.049, 0.34\], ROPE = 0.339, MPE = 0.92). Again, F1 and
F2 did not affect skewness values in the burst (F1: β = −0.016, HDI =
\[−0.058, 0.022\], ROPE = 1, MPE = 0.771; F2: β = −0.011, HDI =
\[−0.051, 0.030\], ROPE = 1, MPE = 0.708).

(ref:plot-monolinguals-summary) Posterior medians ±95% and 66% credible
intervals for VOT and burst metrics of monolingual coronal stops.
Individual point shapes and colors represent the six metrics analyzed.
In this analysis, ‘group’ indicates between-language comparisons of
different speakers.

![(ref:plot-monolinguals-summary)](/Users/casillas/academia/research/in_progress/spanish_english_coronals/figs/mono_summary.pdf)

**Interim discussion.**

General summary and patterns. VOT no surprise RI does nothing spectral
moments relevant in spanish, nothing in english Skewness and kurtosis
pattern similarly, as do COG and standard deviation.

## Experiment 2: Bilinguals

Model info here.

(ref:plot-bilinguals) VOT and burst metrics of coronal stops (/d/, /t/)
from bilingual speakers as a function of language (English, Spanish).
Transparent points represent raw data. Solid points indicate posterior
means ± 95% and 66% credible intervals.

![(ref:plot-bilinguals)](/Users/casillas/academia/research/in_progress/spanish_english_coronals/figs/bi_all_metrics.pdf)

**Voice-onset time.**

(β = 0.523, HDI = \[0.481, 0.566\], ROPE = 0, MPE = 1)

(β = −0.633, HDI = \[−0.738, −0.522\], ROPE = 0, MPE = 1)

(β = 0.025, HDI = \[−0.017, 0.067\], ROPE = 1, MPE = 0.883)

(F1: β = 0.016, HDI = \[−0.013, 0.049\], ROPE = 1, MPE = 0.847; F2: β =
−0.016, HDI = \[−0.053, 0.020\], ROPE = 1, MPE = 0.813).

(β = −0.219, HDI = \[−0.45, −0.001\], ROPE = 0.128, MPE = 0.971)

**Relative intensity.**

(β = 0.097, HDI = \[0.030, 0.161\], ROPE = 0.543, MPE = 0.997)

(β = −0.070, HDI = \[−0.147, 0.011\], ROPE = 0.787, MPE = 0.959)

(β = −0.224, HDI = \[−0.303, −0.139\], ROPE = 0, MPE = 1)

(β = −0.073, HDI = \[−0.134, −0.014\], ROPE = 0.821, MPE = 0.991)

(β = 0.002, HDI = \[−0.066, 0.063\], ROPE = 1, MPE = 0.512)

**Center of gravity.**

(β = 0.661, HDI = \[0.631, 0.692\], ROPE = 0, MPE = 1)

(β = −0.308, HDI = \[−0.340, −0.270\], ROPE = 0, MPE = 1)

(β = 0.096, HDI = \[0.067, 0.126\], ROPE = 0.62, MPE = 1)

(F1: β = 0.036, HDI = \[−0.001, 0.074\], ROPE = 1, MPE = 0.967; F2: β =
−0.015, HDI = \[−0.053, 0.022\], ROPE = 1, MPE = 0.783).

(Spanish /d/ vs. Spanish /t/: β = −0.807, HDI = \[−0.9, −0.71\], ROPE =
0, MPE = 1)

(English /d/ vs. English /t/: β = −0.425, HDI = \[−0.509, −0.335\], ROPE
= 0, MPE = 1)

**Kurtosis.**

(β = −0.635, HDI = \[−0.668, −0.603\], ROPE = 0, MPE = 1)

(β = 0.276, HDI = \[0.237, 0.313\], ROPE = 0, MPE = 1)

(β = −0.173, HDI = \[−0.204, −0.143\], ROPE = 0, MPE = 1)

(F1: β = −0.018, HDI = \[−0.056, 0.022\], ROPE = 1, MPE = 0.808; F2: β =
−0.009, HDI = \[−0.048, 0.029\], ROPE = 1, MPE = 0.666).

(Spanish /d/ vs. Spanish /t/: β = 0.897, HDI = \[0.798, 1.003\], ROPE =
0, MPE = 1)

(English /d/ vs. English /t/: β = 0.207, HDI = \[0.11, 0.3\], ROPE = 0,
MPE = 1)

**Standard deviation.**

(β = 0.568, HDI = \[0.534, 0.604\], ROPE = 0, MPE = 1)

(β = −0.230, HDI = \[−0.282, −0.178\], ROPE = 0, MPE = 1)

(β = 0.082, HDI = \[0.046, 0.115\], ROPE = 0.866, MPE = 1)

(F1: β = 0.000, HDI = \[−0.043, 0.046\], ROPE = 1, MPE = 0.506; F2: β =
0.004, HDI = \[−0.038, 0.050\], ROPE = 1, MPE = 0.571).

(Spanish /d/ vs. Spanish /t/: β = −0.625, HDI = \[−0.754, −0.499\], ROPE
= 0, MPE = 1)

(English /d/ vs. English /t/: β = −0.296, HDI = \[−0.417, −0.174\], ROPE
= 0, MPE = 1)

**Skewness.**

(β = −0.611, HDI = \[−0.642, −0.576\], ROPE = 0, MPE = 1)

(β = 0.298, HDI = \[0.264, 0.337\], ROPE = 0, MPE = 1)

(β = −0.140, HDI = \[−0.169, −0.105\], ROPE = 0, MPE = 1)

(F1: β = −0.029, HDI = \[−0.068, 0.012\], ROPE = 1, MPE = 0.921; F2: β =
−0.008, HDI = \[−0.047, 0.031\], ROPE = 1, MPE = 0.649).

(Spanish /d/ vs. Spanish /t/: β = 0.875, HDI = \[0.774, 0.978\], ROPE =
0, MPE = 1)

(English /d/ vs. English /t/: β = 0.317, HDI = \[0.221, 0.41\], ROPE =
0, MPE = 1)

(ref:plot-bilinguals-summary) Posterior medians ±95% and 66% credible
intervals for VOT and burst metrics of bilingual coronal stops.
Individual point shapes and colors represent the six metrics analyzed.
In this analysis, ‘Language’ refers to a within-participant comparison
between English and Spanish.

![(ref:plot-bilinguals-summary)](/Users/casillas/academia/research/in_progress/spanish_english_coronals/figs/bi_summary.pdf)

## Experiment 3: Bilingual POA data

Model info here.

(ref:plot-poa-bilinguals) VOT and burst metrics of voiceless stops from
bilingual speakers as a function of language (English, Spanish), place
of articulation (Coronal, Bilabial). Transparent points represent raw
data. Solid points indicate posterior means ± 95% and 66% credible
intervals.

![(ref:plot-poa-bilinguals)](/Users/casillas/academia/research/in_progress/spanish_english_coronals/figs/poa_all_metrics.pdf)

**Voice-onset time.**

(β = 0.802, HDI = \[0.750, 0.852\], ROPE = 0, MPE = 1)

(β = 0.134, HDI = \[0.029, 0.236\], ROPE = 0.248, MPE = 0.992)

(β = 0.055, HDI = \[0.004, 0.105\], ROPE = 0.983, MPE = 0.984)

(F1: β = 0.013, HDI = \[−0.025, 0.050\], ROPE = 1, MPE = 0.751; F2: β =
0.000, HDI = \[−0.046, 0.043\], ROPE = 1, MPE = 0.505).

**Relative intensity.**

(β = 0.099, HDI = \[0.042, 0.155\], ROPE = 0.518, MPE = 1)

(β = −0.475, HDI = \[−0.680, −0.269\], ROPE = 0, MPE = 1)

(β = −0.003, HDI = \[−0.056, 0.057\], ROPE = 1, MPE = 0.542)

(β = −0.222, HDI = \[−0.304, −0.136\], ROPE = 0, MPE = 1)

(β = −0.059, HDI = \[−0.116, −0.001\], ROPE = 0.945, MPE = 0.978)

**Center of gravity.**

(β = 0.206, HDI = \[0.177, 0.236\], ROPE = 0, MPE = 1)

(β = 0.953, HDI = \[0.862, 1.041\], ROPE = 0, MPE = 1)

(β = 0.211, HDI = \[0.181, 0.239\], ROPE = 0, MPE = 1)

(F1: β = 0.019, HDI = \[−0.008, 0.049\], ROPE = 1, MPE = 0.905; F2: β =
−0.010, HDI = \[−0.041, 0.020\], ROPE = 1, MPE = 0.743).

(Spanish /t/ vs. English /t/: β = 0.835, HDI = \[0.781, 0.888\], ROPE =
0, MPE = 1)

(Spanish /p/ vs. English /p/: β = −0.01, HDI = \[−0.112, 0.093\], ROPE =
0.976, MPE = 0.574)

**Kurtosis.**

(β = −0.204, HDI = \[−0.237, −0.173\], ROPE = 0, MPE = 1)

(β = −0.888, HDI = \[−1.006, −0.774\], ROPE = 0, MPE = 1)

(β = −0.169, HDI = \[−0.200, −0.137\], ROPE = 0, MPE = 1)

(F1: β = 0.015, HDI = \[−0.016, 0.047\], ROPE = 1, MPE = 0.831; F2: β =
−0.015, HDI = \[−0.047, 0.017\], ROPE = 1, MPE = 0.822).

(Spanish /t/ vs. English /t/: β = −0.747, HDI = \[−0.809, −0.692\], ROPE
= 0, MPE = 1)

(Spanish /p/ vs. English /p/: β = −0.071, HDI = \[−0.186, 0.039\], ROPE
= 0.696, MPE = 0.891)

**Standard deviation.**

(β = 0.230, HDI = \[0.186, 0.271\], ROPE = 0, MPE = 1)

(β = 0.766, HDI = \[0.671, 0.870\], ROPE = 0, MPE = 1)

(β = 0.209, HDI = \[0.169, 0.250\], ROPE = 0, MPE = 1)

(F1: β = −0.012, HDI = \[−0.050, 0.030\], ROPE = 1, MPE = 0.725; F2: β =
0.008, HDI = \[−0.035, 0.049\], ROPE = 1, MPE = 0.662).

(Spanish /t/ vs. English /t/: β = 0.876, HDI = \[0.802, 0.951\], ROPE =
0, MPE = 1)

(Spanish /p/ vs. English /p/: β = 0.042, HDI = \[−0.107, 0.187\], ROPE =
0.783, MPE = 0.722)

**Skewness.**

(β = −0.222, HDI = \[−0.255, −0.188\], ROPE = 0, MPE = 1)

(β = −0.888, HDI = \[−0.994, −0.781\], ROPE = 0, MPE = 1)

(β = −0.192, HDI = \[−0.226, −0.159\], ROPE = 0, MPE = 1)

(F1: β = −0.024, HDI = \[−0.058, 0.009\], ROPE = 1, MPE = 0.924; F2: β =
0.009, HDI = \[−0.025, 0.042\], ROPE = 1, MPE = 0.696).

(Spanish /t/ vs. English /t/: β = −0.829, HDI = \[−0.889, −0.765\], ROPE
= 0, MPE = 1)

(Spanish /p/ vs. English /p/: β = −0.061, HDI = \[−0.176, 0.06\], ROPE =
0.748, MPE = 0.845)

(ref:plot-poa-bilinguals-summary) Posterior medians ±95% and 66%
credible intervals for VOT and burst metrics of bilingual voiceless
bilabial and coronal stops. Individual point shapes and colors represent
the six metrics analyzed. In this analysis, ‘Place’ refers to a
between-participants comparison, as the bilabial data comes from a
separate group of bilingual individuals.

![(ref:plot-poa-bilinguals-summary)](/Users/casillas/academia/research/in_progress/spanish_english_coronals/figs/poa_summary.pdf)

**Interim discussion.**

Figure @ref(fig:plot-poa-bilinguals-summary)

# Discussion

## Summary of findings

## Interpretation and implications

# Conclusion

## Experiment 0: Vowels

Prior to the stop analyses we scrutinized the formant structure, F1 and
F2, of the vowel following the coronal stops. The purpose of this
analysis was to determine if the low /a/ and /æ/ vowels of Spanish and
English, respectively, differed from each other. This analysis was taken
as a precautionary measure with the objective of determining whether or
not coarticulation effects may be present in the stop metrics due to the
possible different spectral envelopes of the next segment.

The F1 and F2 data were analyzed using separate Bayesian multilevel
models. The criterion (F1 or F2) were standardized and modeled as a
function of language (English, Spanish), phoneme (/d/, /t/), and item
repetition. Language and phoneme were sum coded (-1, 1). The random
effects structure included by-subject intercepts with random slopes for
phoneme and item repetition, as well as by-item intercepts with a random
slope for item repetition. The model included weakly informative priors
with the mean centered at 0 and a standard deviation of 2.

The model suggested weak evidence for a language effect on F1 (β =
0.008, HDI = \[−0.221, 0.203\], ROPE = 0.693, MPE = 0.536) and F2 (β =
0.222, HDI = \[−0.007, 0.462\], ROPE = 0.121, MPE = 0.967). Together,
the point estimates suggest the spectral centroid of the Spanish vowel
was slightly higher and more posterior with respect to that of the
English vowel. There was no evidence for a phoneme effect on F1 (β =
0.124, HDI = \[0.007, 0.235\], ROPE = 0.328, MPE = 0.981), nor on F2 (β
= 0.129, HDI = \[−0.042, 0.318\], ROPE = 0.353, MPE = 0.917). Given the
possibility of a small effect of language on the spectral envelope,
standardized F1 and F2 were used in subsequent analyses of the coronal
stops to control for any coarticulatory effects on the burst.
Figure @ref(fig:plot-monolingual-vowels) plots the F1 × F2 data and
Figure @ref(fig:plot-monolingual-vowels-summary) plots the model
summary. A complete summary of the F1 and F2 models is available in
Table @ref(tab:vowel-table).

(ref:plot-monolingual-vowels) F1 × F2 of /a/ from monolingual speakers
as a function of language (English, Spanish). Transparent points
represent raw data. Solid points indicate posterior means ± 95% and 80%
credible intervals.

![(ref:plot-monolingual-vowels)](/Users/casillas/academia/research/in_progress/spanish_english_coronals/figs/vowel_all_metrics_marginal.pdf)

(ref:plot-monolingual-vowels-summary) Posterior medians ± 95% and 66%
credible intervals for F1 and F2 from monolingual speaker data.

![(ref:plot-monolingual-vowels-summary)](/Users/casillas/academia/research/in_progress/spanish_english_coronals/figs/vowel_summary.pdf)

| Metric | Parameter        | Estimate |               HDI |  ROPE |   MPE |
|:-------|:-----------------|---------:|------------------:|------:|------:|
| F1     | Intercept        |   −0.031 | \[−0.283, 0.205\] | 0.587 | 0.606 |
|        | Language         |    0.008 | \[−0.221, 0.203\] | 0.693 | 0.536 |
|        | Phoneme          |    0.124 |  \[0.007, 0.235\] | 0.328 | 0.981 |
|        | Item rep.        |    0.018 | \[−0.050, 0.089\] | 1.000 | 0.689 |
|        | Language:Phoneme |    0.015 | \[−0.097, 0.134\] | 0.951 | 0.595 |
| F2     | Intercept        |   −0.060 | \[−0.289, 0.198\] | 0.563 | 0.692 |
|        | Language         |    0.222 | \[−0.007, 0.462\] | 0.121 | 0.967 |
|        | Phoneme          |    0.129 | \[−0.042, 0.318\] | 0.353 | 0.917 |
|        | Item rep.        |    0.030 | \[−0.045, 0.099\] | 1.000 | 0.802 |
|        | Language:Phoneme |    0.015 | \[−0.172, 0.194\] | 0.757 | 0.566 |

Model summary for F1 and F2 as a function of language (English,
Spanish), phoneme (/d/, /t/), and item repetition for monolingual data.
The percentage of the HDI contained within the ROPE is based on an
effect size of ±0.1.

| Metric | Parameter     | Estimate |                HDI |  ROPE |   MPE |
|:-------|:--------------|---------:|-------------------:|------:|------:|
| VOT    | Intercept     |    0.010 |  \[−0.106, 0.133\] | 0.943 | 0.566 |
|        | Group         |    0.660 |   \[0.556, 0.763\] | 0.000 | 1.000 |
|        | Phoneme       |   −0.617 | \[−0.697, −0.544\] | 0.000 | 1.000 |
|        | F1            |   −0.010 |  \[−0.041, 0.020\] | 1.000 | 0.752 |
|        | F2            |    0.010 |  \[−0.023, 0.042\] | 1.000 | 0.730 |
|        | Item rep.     |   −0.014 |  \[−0.045, 0.018\] | 1.000 | 0.806 |
|        | Group:Phoneme |    0.108 |   \[0.033, 0.180\] | 0.406 | 0.995 |
| COG    | Intercept     |    0.048 |  \[−0.104, 0.185\] | 0.775 | 0.746 |
|        | Group         |    0.687 |   \[0.555, 0.810\] | 0.000 | 1.000 |
|        | Phoneme       |   −0.298 | \[−0.354, −0.244\] | 0.000 | 1.000 |
|        | F1            |    0.005 |  \[−0.033, 0.044\] | 1.000 | 0.590 |
|        | F2            |    0.008 |  \[−0.028, 0.050\] | 1.000 | 0.653 |
|        | Item rep.     |   −0.031 |  \[−0.071, 0.008\] | 1.000 | 0.934 |
|        | Group:Phoneme |    0.214 |   \[0.157, 0.266\] | 0.000 | 1.000 |
| KT     | Intercept     |   −0.026 |  \[−0.163, 0.109\] | 0.864 | 0.640 |
|        | Group         |   −0.646 | \[−0.755, −0.542\] | 0.000 | 1.000 |
|        | Phoneme       |    0.290 |   \[0.221, 0.361\] | 0.000 | 1.000 |
|        | F1            |   −0.027 |  \[−0.067, 0.015\] | 1.000 | 0.903 |
|        | F2            |    0.001 |  \[−0.038, 0.043\] | 1.000 | 0.509 |
|        | Item rep.     |    0.019 |  \[−0.021, 0.062\] | 1.000 | 0.809 |
|        | Group:Phoneme |   −0.263 | \[−0.332, −0.195\] | 0.000 | 1.000 |
| RI     | Intercept     |   −0.111 |  \[−0.347, 0.141\] | 0.446 | 0.820 |
|        | Group         |    0.026 |  \[−0.171, 0.246\] | 0.666 | 0.602 |
|        | Phoneme       |   −0.060 |  \[−0.192, 0.055\] | 0.742 | 0.830 |
|        | F1            |   −0.172 | \[−0.252, −0.085\] | 0.026 | 1.000 |
|        | F2            |   −0.080 |  \[−0.177, 0.015\] | 0.676 | 0.948 |
|        | Item rep.     |    0.051 |  \[−0.023, 0.131\] | 0.916 | 0.912 |
|        | Group:Phoneme |    0.025 |  \[−0.094, 0.157\] | 0.906 | 0.657 |
| SD     | Intercept     |    0.039 |  \[−0.148, 0.213\] | 0.717 | 0.661 |
|        | Group         |    0.490 |   \[0.343, 0.648\] | 0.000 | 1.000 |
|        | Phoneme       |   −0.282 | \[−0.355, −0.204\] | 0.000 | 1.000 |
|        | F1            |   −0.001 |  \[−0.051, 0.051\] | 1.000 | 0.509 |
|        | F2            |    0.007 |  \[−0.044, 0.061\] | 1.000 | 0.595 |
|        | Item rep.     |   −0.027 |  \[−0.077, 0.027\] | 1.000 | 0.833 |
|        | Group:Phoneme |    0.210 |   \[0.141, 0.293\] | 0.000 | 1.000 |
| SK     | Intercept     |   −0.052 |  \[−0.187, 0.084\] | 0.777 | 0.780 |
|        | Group         |   −0.665 | \[−0.774, −0.561\] | 0.000 | 1.000 |
|        | Phoneme       |    0.290 |   \[0.222, 0.362\] | 0.000 | 1.000 |
|        | F1            |   −0.016 |  \[−0.058, 0.022\] | 1.000 | 0.771 |
|        | F2            |   −0.011 |  \[−0.051, 0.030\] | 1.000 | 0.708 |
|        | Item rep.     |    0.032 |  \[−0.009, 0.075\] | 1.000 | 0.933 |
|        | Group:Phoneme |   −0.221 | \[−0.292, −0.155\] | 0.000 | 1.000 |

Model summary for VOT and burst metrics as a function of language
(English, Spanish), phoneme (/d/, /t/), F1, F2, and item repetition for
monolingual coronal stops. The percentage of the HDI contained within
the ROPE is based on an effect size of ± 0.1.

(ref:plot-monolinguals-d-t) Posterior distribution comparing the
short-lag stops, English /d/ and Spanish /t/. The white point represents
the posterior mean ± 95% HDI and the grey region represents the ROPE (±
0.1).

![(ref:plot-monolinguals-d-t)](/Users/casillas/academia/research/in_progress/spanish_english_coronals/figs/mono_post_hoc_dt.pdf)

| Metric | Parameter        | Estimate |                HDI |  ROPE |   MPE |
|:-------|:-----------------|---------:|-------------------:|------:|------:|
| VOT    | Intercept        |   −0.075 |  \[−0.209, 0.059\] | 0.647 | 0.863 |
|        | Language         |    0.523 |   \[0.481, 0.566\] | 0.000 | 1.000 |
|        | Phoneme          |   −0.633 | \[−0.738, −0.522\] | 0.000 | 1.000 |
|        | F1               |    0.016 |  \[−0.013, 0.049\] | 1.000 | 0.847 |
|        | F2               |   −0.016 |  \[−0.053, 0.020\] | 1.000 | 0.813 |
|        | Item rep.        |   −0.009 |  \[−0.038, 0.019\] | 1.000 | 0.728 |
|        | Language:Phoneme |    0.025 |  \[−0.017, 0.067\] | 1.000 | 0.883 |
| COG    | Intercept        |   −0.130 | \[−0.248, −0.003\] | 0.318 | 0.984 |
|        | Language         |    0.661 |   \[0.631, 0.692\] | 0.000 | 1.000 |
|        | Phoneme          |   −0.308 | \[−0.340, −0.270\] | 0.000 | 1.000 |
|        | F1               |    0.036 |  \[−0.001, 0.074\] | 1.000 | 0.967 |
|        | F2               |   −0.015 |  \[−0.053, 0.022\] | 1.000 | 0.783 |
|        | Item rep.        |    0.012 |  \[−0.023, 0.049\] | 1.000 | 0.746 |
|        | Language:Phoneme |    0.096 |   \[0.067, 0.126\] | 0.620 | 1.000 |
| KT     | Intercept        |    0.162 |   \[0.047, 0.291\] | 0.128 | 0.994 |
|        | Language         |   −0.635 | \[−0.668, −0.603\] | 0.000 | 1.000 |
|        | Phoneme          |    0.276 |   \[0.237, 0.313\] | 0.000 | 1.000 |
|        | F1               |   −0.018 |  \[−0.056, 0.022\] | 1.000 | 0.808 |
|        | F2               |   −0.009 |  \[−0.048, 0.029\] | 1.000 | 0.666 |
|        | Item rep.        |   −0.028 |  \[−0.064, 0.010\] | 1.000 | 0.936 |
|        | Language:Phoneme |   −0.173 | \[−0.204, −0.143\] | 0.000 | 1.000 |
| RI     | Intercept        |   −0.020 |  \[−0.217, 0.170\] | 0.711 | 0.582 |
|        | Language         |    0.097 |   \[0.030, 0.161\] | 0.543 | 0.997 |
|        | Phoneme          |   −0.070 |  \[−0.147, 0.011\] | 0.787 | 0.959 |
|        | F1               |   −0.224 | \[−0.303, −0.139\] | 0.000 | 1.000 |
|        | F2               |   −0.073 | \[−0.134, −0.014\] | 0.821 | 0.991 |
|        | Item rep.        |    0.005 |  \[−0.039, 0.053\] | 1.000 | 0.587 |
|        | Language:Phoneme |    0.002 |  \[−0.066, 0.063\] | 1.000 | 0.512 |
| SD     | Intercept        |   −0.111 |  \[−0.240, 0.022\] | 0.426 | 0.951 |
|        | Language         |    0.568 |   \[0.534, 0.604\] | 0.000 | 1.000 |
|        | Phoneme          |   −0.230 | \[−0.282, −0.178\] | 0.000 | 1.000 |
|        | F1               |    0.000 |  \[−0.043, 0.046\] | 1.000 | 0.506 |
|        | F2               |    0.004 |  \[−0.038, 0.050\] | 1.000 | 0.571 |
|        | Item rep.        |    0.014 |  \[−0.024, 0.056\] | 1.000 | 0.750 |
|        | Language:Phoneme |    0.082 |   \[0.046, 0.115\] | 0.866 | 1.000 |
| SK     | Intercept        |    0.116 |  \[−0.014, 0.253\] | 0.403 | 0.959 |
|        | Language         |   −0.611 | \[−0.642, −0.576\] | 0.000 | 1.000 |
|        | Phoneme          |    0.298 |   \[0.264, 0.337\] | 0.000 | 1.000 |
|        | F1               |   −0.029 |  \[−0.068, 0.012\] | 1.000 | 0.921 |
|        | F2               |   −0.008 |  \[−0.047, 0.031\] | 1.000 | 0.649 |
|        | Item rep.        |   −0.008 |  \[−0.046, 0.029\] | 1.000 | 0.664 |
|        | Language:Phoneme |   −0.140 | \[−0.169, −0.105\] | 0.000 | 1.000 |

Model summary for VOT and burst metrics as a function of language
(English, Spanish), phoneme (/d/, /t/), F1, F2, and item repetition for
bilingual coronal stops. The percentage of the HDI contained within the
ROPE is based on an effect size of ± 0.1.

| Metric | Parameter      | Estimate |                HDI |  ROPE |   MPE |
|:-------|:---------------|---------:|-------------------:|------:|------:|
| VOT    | Intercept      |   −0.086 |  \[−0.200, 0.027\] | 0.601 | 0.928 |
|        | Language       |    0.802 |   \[0.750, 0.852\] | 0.000 | 1.000 |
|        | Place          |    0.134 |   \[0.029, 0.236\] | 0.248 | 0.992 |
|        | F1             |    0.013 |  \[−0.025, 0.050\] | 1.000 | 0.751 |
|        | F2             |    0.000 |  \[−0.046, 0.043\] | 1.000 | 0.505 |
|        | Item rep.      |   −0.003 |  \[−0.042, 0.034\] | 1.000 | 0.567 |
|        | Language:Place |    0.055 |   \[0.004, 0.105\] | 0.983 | 0.984 |
| COG    | Intercept      |   −0.569 | \[−0.665, −0.475\] | 0.000 | 1.000 |
|        | Language       |    0.206 |   \[0.177, 0.236\] | 0.000 | 1.000 |
|        | Place          |    0.953 |   \[0.862, 1.041\] | 0.000 | 1.000 |
|        | F1             |    0.019 |  \[−0.008, 0.049\] | 1.000 | 0.905 |
|        | F2             |   −0.010 |  \[−0.041, 0.020\] | 1.000 | 0.743 |
|        | Item rep.      |    0.007 |  \[−0.026, 0.039\] | 1.000 | 0.659 |
|        | Language:Place |    0.211 |   \[0.181, 0.239\] | 0.000 | 1.000 |
| KT     | Intercept      |    0.571 |   \[0.428, 0.690\] | 0.000 | 1.000 |
|        | Language       |   −0.204 | \[−0.237, −0.173\] | 0.000 | 1.000 |
|        | Place          |   −0.888 | \[−1.006, −0.774\] | 0.000 | 1.000 |
|        | F1             |    0.015 |  \[−0.016, 0.047\] | 1.000 | 0.831 |
|        | F2             |   −0.015 |  \[−0.047, 0.017\] | 1.000 | 0.822 |
|        | Item rep.      |   −0.028 |  \[−0.065, 0.011\] | 1.000 | 0.930 |
|        | Language:Place |   −0.169 | \[−0.200, −0.137\] | 0.000 | 1.000 |
| RI     | Intercept      |    0.232 |   \[0.013, 0.434\] | 0.091 | 0.983 |
|        | Language       |    0.099 |   \[0.042, 0.155\] | 0.518 | 1.000 |
|        | Place          |   −0.475 | \[−0.680, −0.269\] | 0.000 | 1.000 |
|        | F1             |   −0.222 | \[−0.304, −0.136\] | 0.000 | 1.000 |
|        | F2             |   −0.059 | \[−0.116, −0.001\] | 0.945 | 0.978 |
|        | Item rep.      |    0.028 |  \[−0.049, 0.103\] | 0.995 | 0.780 |
|        | Language:Place |   −0.003 |  \[−0.056, 0.057\] | 1.000 | 0.542 |
| SD     | Intercept      |   −0.476 | \[−0.594, −0.356\] | 0.000 | 1.000 |
|        | Language       |    0.230 |   \[0.186, 0.271\] | 0.000 | 1.000 |
|        | Place          |    0.766 |   \[0.671, 0.870\] | 0.000 | 1.000 |
|        | F1             |   −0.012 |  \[−0.050, 0.030\] | 1.000 | 0.725 |
|        | F2             |    0.008 |  \[−0.035, 0.049\] | 1.000 | 0.662 |
|        | Item rep.      |    0.016 |  \[−0.033, 0.061\] | 1.000 | 0.748 |
|        | Language:Place |    0.209 |   \[0.169, 0.250\] | 0.000 | 1.000 |
| SK     | Intercept      |    0.527 |   \[0.410, 0.656\] | 0.000 | 1.000 |
|        | Language       |   −0.222 | \[−0.255, −0.188\] | 0.000 | 1.000 |
|        | Place          |   −0.888 | \[−0.994, −0.781\] | 0.000 | 1.000 |
|        | F1             |   −0.024 |  \[−0.058, 0.009\] | 1.000 | 0.924 |
|        | F2             |    0.009 |  \[−0.025, 0.042\] | 1.000 | 0.696 |
|        | Item rep.      |    0.000 |  \[−0.037, 0.039\] | 1.000 | 0.518 |
|        | Language:Place |   −0.192 | \[−0.226, −0.159\] | 0.000 | 1.000 |

Model summary for VOT and burst metrics as a function of language
(English, Spanish), place of articulation (bilabial, coronal), F1, F2,
and item repetition for bilingual voiceless stops. The percentage of the
HDI contained within the ROPE is based on an effect size of ± 0.1.

## Bayesian data analysis

This study employs Bayesian Data Analysis for quantitative inferential
statistics. Specifically, this implies that we use Bayesian *credible
intervals*—and other metrics—to draw statistical inferences. A Bayesian
model calculates a posterior distribution, i.e., a distribution of
plausible parameter values, given the data, a data-generating model, and
any prior information we have about those parameter values. Posterior
distributions are computationally costly. For this reason, we use the
Hamiltonian Markov Chain Monte Carlo algorithm to obtain a sample that
incldues thousands of values from the posterior distribution. In
practical terms, what this means is that we do not calculate a single
point estimate for an effect β, but rather we draw a sample of 4,000
plausible values for β. This allows us to quantify our uncertainty
regarding β by summarizing the distribution of those values. We will use
4 statistics to describe the posterior distribution: (1) the mean, (2)
the highest density credible interval (HDI), (3) the proportion of the
HDI that falls within a Region of Practical Equivalence (ROPE), and (4)
the Maximum Probability of Effect (MPE). The mean provides a point
estimate for the distribution. The 95% highest density credible interval
provides bounds for the effect. The ROPE designates a region of
practical equivalence for a negligible effect and calculates the
proportion of the HDI that falls within this interval.[2] The MPE
calculates the proportion of the posterior distribution that is of the
median’s sign (or the probability that the effect is positive or
negative).

If, for instance, a hypothesis states that β \> 0, we judge there to be
*compelling evidence* for this hypothesis if the mean point estimate is
a positive number, if the 95% credible interval of β does not contain 0
and is outside the ROPE by a reasonably clear margin, and the posterior
*P*(β \> 0) is close to one. Together these four statistics allow us to
quantify our uncertainty and provide an intuitive interpretation of any
given effect. Consider a case in which the posterior mean of β is 100
and the 95% credible interval is \[40, 160\]. The interval tells us that
we can be 95% certain the *true* value of β is between 40 and 160, given
the data, our model, and our prior information. Furthermore, the
interval allows us to specify areas of uncertainty. In this example, we
can conclude that the effect is almost certain to be positive. The lower
interval value of 40 tells us that 95% of the plausible values are
greater than 40. We also note that the interval covers a wide range of
values, thus we also conclude that we are not very certain about the
size of the effect. This type of interpretation is not possible under a
frequentist paradigm.

## About this document

This document was written in RMarkdown using `papaja` (Aust & Barth,
2018) and serves as a project report for our research group. The
document is written as if it were the results section of a future
manuscript. This implies that it is written in a way that allows it to
be copy and pasted into the actual manuscript once it is available.

## Session info

    >   setting  value
    >   version  R version 4.1.3 (2022-03-10)
    >   os       macOS Big Sur/Monterey 10.16
    >   system   x86_64, darwin17.0
    >   ui       X11
    >   language (EN)
    >   collate  en_US.UTF-8
    >   ctype    en_US.UTF-8
    >   tz       America/New_York
    >   date     2022-04-02
    >   pandoc   2.14.2 @ /Applications/RStudio.app/Contents/MacOS/pandoc/ (via rmarkdown)

    >                 loadedversion       date
    >  abind                  1.4-5 2016-07-21
    >  academicWriteR         0.4.1 2021-06-05
    >  arrayhelpers           1.1-0 2020-02-04
    >  assertthat             0.2.1 2019-03-21
    >  backports              1.4.1 2021-12-13
    >  base64enc              0.1-3 2015-07-28
    >  bayesplot              1.9.0 2022-03-10
    >  bayestestR            0.11.5 2021-10-30
    >  beeswarm               0.4.0 2021-06-01
    >  bit                    4.0.4 2020-08-04
    >  bit64                  4.0.5 2020-08-30
    >  bookdown                0.25 2022-03-16
    >  boot                  1.3-28 2021-05-03
    >  bridgesampling         1.1-2 2021-04-16
    >  brio                   1.1.3 2021-11-30
    >  brms                  2.16.3 2021-11-22
    >  Brobdingnag            1.2-7 2022-02-03
    >  broom                 0.7.12 2022-01-28
    >  cachem                 1.0.6 2021-08-19
    >  callr                  3.7.0 2021-04-20
    >  checkmate              2.0.0 2020-02-06
    >  cli                    3.2.0 2022-02-14
    >  coda                  0.19-4 2020-09-30
    >  codetools             0.2-18 2020-11-04
    >  colorspace             2.0-3 2022-02-21
    >  colourpicker           1.1.1 2021-10-04
    >  crayon                 1.5.0 2022-02-14
    >  crosstalk              1.2.0 2021-11-04
    >  curl                   4.3.2 2021-06-23
    >  data.table            1.14.2 2021-09-27
    >  datawizard             0.3.0 2022-03-03
    >  DBI                    1.1.2 2021-12-20
    >  desc                   1.4.1 2022-03-06
    >  devtools               2.4.3 2021-11-30
    >  digest                0.6.29 2021-12-01
    >  distributional         0.3.0 2022-01-05
    >  dplyr                  1.0.8 2022-02-08
    >  DT                      0.21 2022-02-26
    >  dygraphs             1.1.1.6 2018-07-11
    >  ellipsis               0.3.2 2021-04-29
    >  emmeans                1.7.2 2022-01-04
    >  estimability             1.3 2018-02-11
    >  evaluate                0.15 2022-02-18
    >  fansi                  1.0.2 2022-01-14
    >  farver                 2.1.0 2021-02-28
    >  fastmap                1.1.0 2021-01-25
    >  flextable              0.7.0 2022-03-06
    >  forcats                0.5.1 2021-01-27
    >  fs                     1.5.2 2021-12-08
    >  future                1.24.0 2022-02-19
    >  gamm4                  0.2-6 2020-04-03
    >  gdtools                0.2.4 2022-02-14
    >  generics               0.1.2 2022-01-31
    >  ggbeeswarm             0.6.0 2017-08-07
    >  ggdist                 3.1.1 2022-02-27
    >  ggExtra                  0.9 2019-08-27
    >  ggplot2                3.3.5 2021-06-25
    >  ggridges               0.5.3 2021-01-08
    >  ggstance               0.3.5 2020-12-17
    >  globals               0.14.0 2020-11-22
    >  glue                   1.6.2 2022-02-24
    >  gridExtra                2.3 2017-09-09
    >  gtable                 0.3.0 2019-03-25
    >  gtools                 3.9.2 2021-06-06
    >  here                   1.0.1 2020-12-13
    >  highr                    0.9 2021-04-16
    >  hms                    1.1.1 2021-09-26
    >  htmltools              0.5.2 2021-08-25
    >  htmlwidgets            1.5.4 2021-09-08
    >  httpuv                 1.6.5 2022-01-05
    >  igraph                1.2.11 2022-01-04
    >  inline                0.3.19 2021-05-31
    >  insight               0.16.0 2022-02-17
    >  jsonlite               1.8.0 2022-02-22
    >  knitr                   1.37 2021-12-16
    >  later                  1.3.0 2021-08-18
    >  lattice              0.20-45 2021-09-22
    >  lifecycle              1.0.1 2021-09-24
    >  listenv                0.8.0 2019-12-05
    >  lme4                  1.1-28 2022-02-05
    >  loo                    2.4.1 2020-12-09
    >  magrittr               2.0.2 2022-01-26
    >  markdown                 1.1 2019-08-07
    >  MASS                  7.3-55 2022-01-16
    >  Matrix                 1.4-0 2021-12-08
    >  matrixStats           0.61.0 2021-09-17
    >  memoise                2.0.1 2021-11-26
    >  mgcv                  1.8-39 2022-02-24
    >  mime                    0.12 2021-09-28
    >  miniUI               0.1.1.1 2018-05-18
    >  minqa                  1.2.4 2014-10-09
    >  modelr                 0.1.8 2020-05-19
    >  munsell                0.5.0 2018-06-12
    >  mvtnorm                1.1-3 2021-10-08
    >  nlme                 3.1-155 2022-01-16
    >  nloptr                 2.0.0 2022-01-26
    >  officer                0.4.1 2021-11-14
    >  parallelly            1.30.0 2021-12-17
    >  pillar                 1.7.0 2022-02-01
    >  pkgbuild               1.3.1 2021-12-20
    >  pkgconfig              2.0.3 2019-09-22
    >  pkgload                1.2.4 2021-11-30
    >  plyr                   1.8.6 2020-03-03
    >  posterior              1.2.1 2022-03-07
    >  prettyunits            1.1.1 2020-01-24
    >  processx               3.5.2 2021-04-30
    >  projpred               2.0.2 2020-10-28
    >  promises             1.2.0.1 2021-02-11
    >  ps                     1.6.0 2021-02-28
    >  purrr                  0.3.4 2020-04-17
    >  R6                     2.5.1 2021-08-19
    >  Rcpp                 1.0.8.2 2022-03-11
    >  RcppParallel           5.1.5 2022-01-05
    >  readr                  2.1.2 2022-01-30
    >  remotes                2.4.2 2021-11-30
    >  reshape2               1.4.4 2020-04-09
    >  rlang                  1.0.2 2022-03-04
    >  rmarkdown               2.13 2022-03-10
    >  rprojroot              2.0.2 2020-11-15
    >  rstan                 2.26.4 2021-10-18
    >  rstantools             2.1.1 2020-07-06
    >  rstudioapi              0.13 2020-11-12
    >  scales                 1.1.1 2020-05-11
    >  sessioninfo            1.2.2 2021-12-06
    >  shiny                  1.7.1 2021-10-02
    >  shinyjs                2.1.0 2021-12-23
    >  shinystan              2.6.0 2022-03-03
    >  shinythemes            1.2.0 2021-01-25
    >  StanHeaders           2.26.4 2021-10-18
    >  stringi                1.7.6 2021-11-29
    >  stringr                1.4.0 2019-02-10
    >  svUnit                 1.0.6 2021-04-19
    >  systemfonts            1.0.4 2022-02-11
    >  tensorA               0.36.2 2020-11-19
    >  testthat               3.1.2 2022-01-20
    >  threejs                0.3.3 2020-01-21
    >  tibble                 3.1.6 2021-11-07
    >  tidybayes              3.0.2 2022-01-05
    >  tidyr                  1.2.0 2022-02-01
    >  tidyselect             1.1.2 2022-02-21
    >  tzdb                   0.2.0 2021-10-27
    >  usethis                2.1.5 2021-12-09
    >  utf8                   1.2.2 2021-07-24
    >  uuid                   1.0-4 2022-03-16
    >  V8                     4.1.0 2022-02-06
    >  vctrs                  0.3.8 2021-04-29
    >  vipor                  0.4.5 2017-03-22
    >  viridis                0.6.2 2021-10-13
    >  viridisLite            0.4.0 2021-04-13
    >  vroom                  1.5.7 2021-11-30
    >  withr                  2.5.0 2022-03-03
    >  xfun                    0.30 2022-03-02
    >  xml2                   1.3.3 2021-11-30
    >  xtable                 1.8-4 2019-04-21
    >  xts                   0.12.1 2020-09-09
    >  yaml                   2.3.5 2022-02-21
    >  zip                    2.2.0 2021-05-31
    >  zoo                    1.8-9 2021-03-09

# References

<div id="refs" class="references csl-bib-body hanging-indent"
line-spacing="2">

<div id="ref-R-papaja" class="csl-entry">

Aust, F., & Barth, M. (2018). *<span class="nocase">papaja</span>:
Create APA manuscripts with R Markdown*. Retrieved from
<https://github.com/crsh/papaja>

</div>

<div id="ref-R-brms_a" class="csl-entry">

Bürkner, P.-C. (2017). <span class="nocase">brms</span>: An R package
for Bayesian multilevel models using Stan. *Journal of Statistical
Software*, *80*(1), 1–28. <https://doi.org/10.18637/jss.v080.i01>

</div>

<div id="ref-R-brms_b" class="csl-entry">

Bürkner, P.-C. (2018). Advanced Bayesian multilevel modeling with the R
package <span class="nocase">brms</span>. *The R Journal*, *10*(1),
395–411. <https://doi.org/10.32614/RJ-2018-017>

</div>

<div id="ref-cohen1988statistical" class="csl-entry">

Cohen, J. (1988). *Statistical power analysis for the behavioral
sciences*. Hillsdale, NJ: Erlbaum.

</div>

<div id="ref-cohen2013statistical" class="csl-entry">

Cohen, J. (2013). *Statistical power analysis for the behavioral
sciences*. Routledge.

</div>

<div id="ref-Gelman_2017" class="csl-entry">

Gelman, A., Simpson, D., & Betancourt, M. (2017). The prior can often
only be understood in the context of the likelihood. *Entropy*,
*19*(10), 1–13. <https://doi.org/10.3390/e19100555>

</div>

<div id="ref-kruschke2018rejecting" class="csl-entry">

Kruschke, J. K. (2018). Rejecting or accepting parameter values in
bayesian estimation. *Advances in Methods and Practices in Psychological
Science*, *1*(2), 270–280.

</div>

<div id="ref-R-base" class="csl-entry">

R Core Team. (2019). *R: A language and environment for statistical
computing*. Vienna, Austria: R Foundation for Statistical Computing.
Retrieved from <https://www.R-project.org/>

</div>

<div id="ref-van2014bayesian" class="csl-entry">

Schoot, R. van de, & Depaoli, S. (2014). Bayesian analyses: Where to
start and what to report. *European Health Psychologist*, *16*(2),
75–84.

</div>

<div id="ref-vasishth2018bayesian" class="csl-entry">

Vasishth, S., Nicenboim, B., Beckman, M. E., Li, F., & Kong, E. J.
(2018). Bayesian data analysis in the phonetic sciences: A tutorial
introduction. *Journal of Phonetics*, *71*, 147–161.

</div>

</div>

[1] See Schoot & Depaoli (2014) and Vasishth, Nicenboim, Beckman, Li, &
Kong (2018) for tutorials and in depth explanations related to BDA in
the psychological and speech sciences.

[2] We utilize a ROPE of ± 1 for standardized values. For
non-standardized values Kruschke (2018) recommends using the formula in
@ref(eq:rope)
