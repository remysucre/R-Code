* This file replicates the Stata analysis in Nyhan, McGhee, Sides, Masket, and Greene (APR 2012).
* Until the 2010 CCES common content is made public, it requires access to those data.

* This code replicates Tables 1 and 2 as well as Figures 1 and 2

/*MERGE HOUSE DATA WITH CCES*/
* prepare House data for merge
use "cd-for-replication.dta", clear
keep jacstate district incumb unc dpvote health2 stimulus gwarm dwnom1
drop if jacstate==38 & district==12 /*PA 12: Murtha cast crucial roll calls then died; replacement, Critz, didn't vote*/
drop if jacstate==9 & district==19 /*FL 19: Wexler resigned; Dem Ted Deutch elected in April 2010*/
sort jacstate district
save temp, replace

* open subset of CCES
* note: full 2010 CCES Common Content should be released publicly by 2012
use "cces-for-replication.dta", clear

* destring state FIPS and congressional district variables
destring v276 v302, replace
rename v276 district

* for states with only 1 district, recode district to 1
recode district 0=1

* creates jacstate variable in CCES data
* allows merge of aggregate House data into CCES individual-level data
gen jacstate=v206
* AL=1 AK=2 can stay the same
recode jacstate 4=3 /*AZ*/
recode jacstate 5=4 /*AR*/
recode jacstate 6=5 /*CA*/
recode jacstate 8=6 /*CO*/
recode jacstate 9=7 /*CT*/
recode jacstate 10=8 /*DE*/
recode jacstate 11=. /*DC*/
recode jacstate 12=9 /*FL*/
recode jacstate 13=10 /*GA*/
recode jacstate 15=11 /*HI*/
recode jacstate 16=12 /*ID*/
recode jacstate 17=13 /*IL*/
recode jacstate 18=14 /*IN*/
recode jacstate 19=15 /*IA*/
recode jacstate 20=16 /*KS*/
recode jacstate 21=17 /*KY*/
recode jacstate 22=18 /*LA*/
recode jacstate 23=19 /*ME*/
recode jacstate 24=20 /*MD*/
recode jacstate 25=21 /*MA*/
recode jacstate 26=22 /*MI*/
recode jacstate 27=23 /*MN*/
recode jacstate 28=24 /*MS*/
recode jacstate 29=25 /*MO*/
recode jacstate 30=26 /*MT*/
recode jacstate 31=27 /*NE*/
recode jacstate 32=28 /*NV*/
recode jacstate 33=29 /*NH*/
recode jacstate 34=30 /*NJ*/
recode jacstate 35=31 /*NM*/
recode jacstate 36=32 /*NY*/
recode jacstate 37=33 /*NC!*/
recode jacstate 38=34 /*ND*/
recode jacstate 39=35 /*OH*/
recode jacstate 40=36 /*OK*/
recode jacstate 41=37 /*OR*/
recode jacstate 42=38 /*PA*/
recode jacstate 44=39 /*RI*/
recode jacstate 45=40 /*SC*/
recode jacstate 46=41 /*SD*/
recode jacstate 47=42 /*TN*/
recode jacstate 48=43 /*TX*/
recode jacstate 49=44 /*UT*/
recode jacstate 50=45 /*VT*/
recode jacstate 51=46 /*VA*/
recode jacstate 53=47 /*WA*/
recode jacstate 54=48 /*WV*/
recode jacstate 55=49 /*WI*/
recode jacstate 56=50 /*WY*/

* merge in aggregate data
sort jacstate district
merge jacstate district using temp

* drop R's from DC (N=157)
* also drop Rs from districts where Democratic representatives were elected in special elections in 2010 (N=131 from PA-12 and N=154 from FL-9)
drop if _m==1 
drop _m

* isolate Rs who had a Dem House incumbent running in a contested race
keep if incumb==1 & unc==0 /*contested Dem incumbents*/

* identification number for each member
* use fips + cd
gen id=(v302*100)+district

* weight
gen weight=v101

* perceptions of Dem MC's ideology
recode cc334j 8=., gen(ideoldem)
recode cc334j 1/7=1 8=0, gen(ideoldempl) /*can place Dem on scale*/

* own ideology
recode cc334a 8=., gen(ideolself)
gen ideoldiff=ideoldem-ideolself

* recode ideoldiff
* collapse higher values since there are few Rs and nearly all are loyal Dems
* then multiply by -1 so that higher values signal that R is more conservative than Dem incumb
recode ideoldiff 1/6=0, gen(ideoldiff2)
replace ideoldiff2=ideoldiff2*-1

* pid measure
recode v212d 8=., gen(pid7)
recode v212d 1=3 2=2 3=1 4=0 5=1 6=2 7=3 8=., gen(partisan) /*strength of partisanship, i.e., folded measure*/
recode v212d 1/3=1 4=2 5/7=3 *=., gen(pid3)
label define pid3 1 "Dem" 2 "Ind" 3 "Rep"
label values pid3 pid3
recode pid3 1=1 2=0 3=0, gen(dem)
recode pid3 1=0 2=1 3=0, gen(ind)
recode pid3 3=1 1=0 2=0, gen(rep)

* approval of member
recode cc315a 1=4 2=3 3=2 4=1 5=., gen(mcapprove)
label variable mcapprove "Job approval of MC"

* house vote
recode cc412 1=1 2=0 *=., gen(housevote)
label define housevote 1 "Dem" 0 "Rep"
label values housevote housevote
label var housevote "House vote"

*  variable and value labels
label define rollcall 0 "Oppose" 1 "Support"
label values health2 rollcall
label values stimulus rollcall
label values gwarm rollcall
label var health2 "Health Care Reform"
label var stimulus "Stimulus"
label var gwarm "Cap and trade"
label var ideoldem "Ideological placement of Democratic incumbent"
label define ideol 1 "Very liberal" 4 "Middle of the road" 7 "Very conservative"
label values ideoldem ideol
label var ideoldiff "Perceived ideological difference"
label var ideoldiff2 "Perceived ideological difference"
label var pid7 "Party identification"
label var dwnom1 "DW-NOMINATE score (1st dimension)"

* FIGURE 1: DENSITIES OF IDEOLOGY
kdensity ideoldem if health2==1, at(ideoldem) bwidth(.95) gen(i1)
kdensity ideoldem if health2==0, at(ideoldem) bwidth(.95) gen(i2)
line i2 i1 ideoldem, sort ylab(0(.05).25,labsize(*.8) nogrid) ytitle("Density",size(*.8)) legend(lab(1 "Opponents") lab(2 "Supporters") region(ls(none)) size(*.8)) name(zero,replace) title("Health care reform",size(*.8)) scheme(s2mono) xlab(1 `""Very" "liberal""' 4 `""Middle of" "the road""' 7 `""Very" "conservative""',labsize(*.8)) xtick(1(1)7) xtitle("") xscale(r(.9 7.2))

kdensity ideoldem if stimulus==1, at(ideoldem) bwidth(.95) gen(i3)
kdensity ideoldem if stimulus==0, at(ideoldem) bwidth(.95) gen(i4)
line i4 i3 ideoldem, sort ylab(none,nogrid) ytick(0(.05).25) ytitle("",size(*.8)) legend(lab(1 "Opponents") lab(2 "Supporters") size(*.8)) name(one,replace) title("Stimulus",size(*.8)) scheme(s2mono) xlab(1 `""Very" "liberal""' 4 `""Middle of" "the road""' 7 `""Very" "conservative""',labsize(*.8)) xtick(1(1)7) xtitle("") xscale(r(.9 7.2))

kdensity ideoldem if gwarm==1, at(ideoldem) bwidth(.95) gen(i5)
kdensity ideoldem if gwarm==0, at(ideoldem) bwidth(.95) gen(i6)
line i6 i5 ideoldem, sort ylab(0(.05).25,nogrid labsize(*.8)) ytitle("Density",size(*.8)) legend(lab(1 "Opponents") lab(2 "Supporters") region(ls(none)) size(*.8)) name(two,replace) title("Cap and trade",size(*.8)) scheme(s2mono) xlab(1 `""Very" "liberal""' 4 `""Middle of" "the road""' 7 `""Very" "conservative""',labsize(*.8)) xtick(1(1)7) xtitle("") xscale(r(.9 7.2))

grc1leg zero one two, legendfrom(zero) graphregion(fcolor(white) ifcolor(none)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) imargin(medium)

* FIGURE 2: DENSITY OF IDEOLOGY DIFFERENCE
kdensity ideoldiff, bwidth(.5) lcolor(black) ///
xsc(r(-6 6)) xtick(0) ///
xlab(-3.5 `""More conservative" "than member""' 0 `""Same as" "member""' 3.5 `""More liberal" "than member""', notick) ///
plotr(ls(none) margin(b=0)) graphr(margin(t=10)) ylab(0(.1).2,angle(0)) note("") xtitle("") title("") ///
xline(.32) text(.218 .32 "Average", size(small)) text(.21 .32 "Democrat", size(small)) ///
xline(-1.856583) text(.218 -1.856583 "Average", size(small)) text(.21 -1.856583 "independent", size(small)) ///
xline(-3.98) text(.218 -3.98 "Average", size(small)) text(.21 -3.98 "Republican", size(small))

* ttests reported in text
ttest ideoldem, by(tarp) unequal
ttest ideoldem, by(stimulus) unequal
ttest ideoldem, by(gwarm) unequal
ttest ideoldem, by(health2) unequal

* TABLE 1: MODEL OF IDEOLOGICAL PERCEPTIONS
* set survey
svyset id [pweight=weight]

* models on entire sample reported in Table 1
svy: regress ideoldem pid7 dwnom1 health2 stimulus gwarm
svy: regress ideoldiff pid7 dwnom1 health2 stimulus gwarm

* models without svy to get unweighted Ns reported in Table 1
regress ideoldem pid7 dwnom1 health2 stimulus gwarm
regress ideoldiff pid7 dwnom1 health2 stimulus gwarm 

* models by party reported in text
svy: regress ideoldiff pid7 dwnom1 health2 stimulus gwarm if dem==1
svy: regress ideoldiff dwnom1 health2 stimulus gwarm if ind==1
svy: regress ideoldiff pid7 dwnom1 health2 stimulus gwarm if rep==1

* add control for party unity
* results reported in footnote 4
svy: regress ideoldem pid7 unity dwnom1 health2 stimulus gwarm
svy: regress ideoldiff pid7 unity dwnom1 health2 stimulus gwarm

* TABLE 2: MODELS OF HOUSE VOTE
svy: probit housevote pid7 dpvote health2 stimulus gwarm dwnom1 if ideoldiff2~=.
svy: probit housevote ideoldiff2 pid7 dpvote health2 stimulus gwarm dwnom1
svy: probit housevote partisan dpvote health2 stimulus gwarm dwnom1 if ideoldiff2~=. & dem==1
svy: probit housevote ideoldiff2 partisan dpvote health2 stimulus gwarm dwnom1 if dem==1
svy: probit housevote dpvote health2 stimulus gwarm dwnom1 if ideoldiff2~=. & ind==1
svy: probit housevote ideoldiff2 dpvote health2 stimulus gwarm dwnom1 if ind==1
svy: probit housevote partisan dpvote health2 stimulus gwarm dwnom1 if ideoldiff2~=. & rep==1
svy: probit housevote ideoldiff2 partisan dpvote health2 stimulus gwarm dwnom1 if rep==1

* unweighted Ns reported in Table 2
probit housevote pid7 dpvote health2 stimulus gwarm dwnom1 if ideoldiff2~=.
probit housevote partisan dpvote health2 stimulus gwarm dwnom1 if ideoldiff2~=. & dem==1
probit housevote dpvote health2 stimulus gwarm dwnom1 if ideoldiff2~=. & ind==1
probit housevote partisan dpvote health2 stimulus gwarm dwnom1 if ideoldiff2~=. & rep==1



*This code replicates Figure 3, Table 3, Table A-3, and Table B-1

use "cd-for-replication.dta",clear

*Figure 3

twoway (scatter pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & health2==1,mcolor(gs7) msymbol(triangle)) (lfit pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & health2==1,lcolor(gs7) lpattern(dash) legend(order(3 1) lab(3 "Opponents") lab(1 "Supporters") size(*.6)) title("Health care reform",size(*.8)) xtitle("Obama vote",size(*.8) color(white)) xlab(25 "25%" 50 "50%" 75 "75%" 100 "100%",labsize(*.8)) yscale(r(30 95)) ytitle("Incumbent two-party vote",size(*.8)) ylab(30 "30%" 50 "50%" 70 "70%" 90 "90%",labsize(*.8)) ymtick(40(20)80)) (scatter pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & health2==0,msymbol(circle) mcolor(black) xscale(r(30 100))) (lfit pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & health2==0,lcolor(black) name(zero,replace))   

twoway (scatter pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & stimulus==1,mcolor(gs7) msymbol(triangle)) (lfit pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & stimulus ==1,lcolor(gs7) lpattern(dash) legend(order(3 1) lab(3 "Opponents") lab(1 "Supporters") size(*.9)) yscale(r(30 95)) xtitle("Obama vote",size(*.8)) title("Stimulus",size(*.8)) xlab(25 "25%" 50 "50%" 75 "75%" 100 "100%",labsize(*.8)) ytitle("") ylab(none)) (scatter pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & stimulus ==0,msymbol(circle) mcolor(black) xscale(r(30 100))) (lfit pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & stimulus ==0,lcolor(black) name(one,replace))  

twoway (scatter pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & gwarm==1,mcolor(gs7) msymbol(triangle)) (lfit pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & gwarm ==1,lcolor(gs7) lpattern(dash) legend(order(3 1) lab(3 "Opponents") lab(1 "Supporters") size(*.9)) yscale(r(30 95)) title("Cap and trade",size(*.8)) xtitle("Obama vote", color(white) size(*.8)) xlab(25 "25%" 50 "50%" 75 "75%" 100 "100%",labsize(*.8)) ytitle("") ylab(none))  (scatter pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & gwarm ==0,msymbol(circle) mcolor(black) xscale(r(30 100))) (lfit pcdemvt dpvote if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1 & gwarm ==0,lcolor(black) name(two,replace))  

grc1leg zero one two, legendfrom(zero) graphregion(fcolor(white) ifcolor(none)) plotregion(fcolor(none) lcolor(white) ifcolor(none) ilcolor(none)) imargin(medsmall) rows(1)

*Table 3 (requires installation of estout)

reg pcdemvt dpvote unity dwnom1 health2 stimulus gwarm if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1,robust
est store A

reg pcdemvt dpvote unity dwnom1 health2 if healthmatch==1 [pweight=healthweights], robust 
est store B

reg pcdemvt dpvote unity dwnom1 stimulus if stimulusmatch==1 [pweight=stimweights], robust 
est store C

reg pcdemvt dpvote unity dwnom1 gwarm if gwarmmatch==1 [pweight=gwarmweights], robust 
est store D

estout A B C D, replace varwidth(25) collabels("") cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N, fmt(%9.2f %9.0f) labels("R2" "N")) starlevels(* 0.05)

*Table A-3 (requires installation of estout)
keep if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1
bysort health2: su dpvote 
bysort health2: su dpvote [aweight=healthweights]
bysort health2: su unity 
bysort health2: su unity [aweight=healthweights]
bysort health2: su dwnom1 
bysort health2: su dwnom1 [aweight=healthweights]

bysort stimulus: su dpvote 
bysort stimulus: su dpvote [aweight=stimweights]
bysort stimulus: su unity 
bysort stimulus: su unity [aweight=stimweights]
bysort stimulus: su dwnom1 
bysort stimulus: su dwnom1 [aweight=stimweights]

bysort gwarm: su dpvote 
bysort gwarm: su dpvote [aweight=gwarmweights]
bysort gwarm: su unity 
bysort gwarm: su unity [aweight=gwarmweights]
bysort gwarm: su dwnom1 
bysort gwarm: su dwnom1 [aweight=gwarmweights]

*Table B-1 (requires installation of estout)

*rescale housedemcand08
replace housedemcand08=housedemcand08/10

reg housedemcand10 housedemcand08 unity dwnom1 health2 stimulus gwarm if pcdemvt <100 & pcdemvt >0 & party2 == "dem" & incumb==1, robust 
est store A

reg housedemcand10 housedemcand08 unity dwnom1 health2 if healthmatch==1 [pweight=healthweights], robust 
est store B

reg housedemcand10 housedemcand08 unity dwnom1 stimulus if stimulusmatch==1 [pweight=stimweights], robust
est store C

reg housedemcand10 housedemcand08 unity dwnom1 gwarm  if gwarmmatch==1 [pweight=gwarmweights], robust
est store D

estout A B C D, replace varwidth(25) collabels("") cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) stats(r2 N, fmt(%9.2f %9.0f) labels("R2" "N")) starlevels(* 0.05)
