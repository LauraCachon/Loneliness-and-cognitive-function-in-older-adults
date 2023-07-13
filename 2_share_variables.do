*set maxvar 15000

clear
cd "/Users/kjkomula/Documents/share_all_data/"

*https://www.share-datadocutool.org/data_variables/view/18944

foreach v in 4 5 6 7 8 {
	clear
	use share`v'
	di `v'

    * Interview year
    recode int_year (-9=.)
    
    * Year of birth
    recode yrbirth (-1=.)
    
    * Sex
    gen sex=gender-1
    
    * Age  
    gen age1=int_year-yrbirth
    summ age1
	
	* Marital status
    recode dn014_ (-9/-1=.) (1/3=0) (4/6=1), gen(msd1) 
    // 1 – not married/in a registered partnership, 0 – married/in registered partnership
	
	* Partner in household
	recode partnerinhh (-9=.) (3=1) (1=0), gen(partner1)
	tab partnerinhh partner1
	// 1 - no partner, 0 - partner
    
    * Education
    recode isced1997_r (-2/-1=.) (95/97=.), gen(edu1)
    // 0 - None, 1–6 – ISCED-97 levels 1–6
    
    * Diabetes
    recode ph006d5 (-2/-1=.), gen(diab1)
    replace diab1=1 if ph011d6==1
    // 1 - ever diagnosed or currently having or drugs to treat
    
    * High blood pressure 
    recode ph006d2 (-2/-1=.), gen(bp1)
    replace bp1=1 if ph011d2==1
    // 1 - ever diagnosed or currently having or drugs to treat
    
    * Stroke
    recode ph006d4 (-2/-1=.), gen(stroke1)
    // 1 - ever diagnosed or currently having
    
    * Heart attack
    recode ph006d1 (-2/-1=.), gen(chd1) 
    // 1 - ever diagnosed or currently having
	
	* Cancer
    recode ph006d10 (-2/-1=.), gen(cancer1) 
	// 1 - ever diagnosed or currently having
    
	* Chronic lung disease
	recode ph006d6 (-2/-1=.), gen(resp1) 
    // 1 - ever diagnosed or currently having
    
	* Disability (1+ adl limitations)
	tab adl2, nolab
	recode adl2 (-2/-1=.), gen(disab1)

	
	save share_red`v', replace

	}

	
// Loneliness
// 0 - never, 1 - some of the time, 2 - often

foreach v in 5 6 7 8 {
	clear
	use share_red`v'
	di `v'

	recode mh034_ (-2/-1=.) (3=0) (2=1) (1=2), gen(nocompanion)
	
	
    recode mh035_ (-2/-1=.) (3=0) (2=1) (1=2), gen(leftout)
    recode mh036_ (-2/-1=.) (3=0) (2=1) (1=2), gen(isolated)
	recode mh037_ (-2/-1=.) (3=0) (2=1) (1=2), gen(lonely)

	save share_red`v', replace
	
	}
	

// APPEND

clear
use share_red4
keep mergeid int_year yrbirth country sex age1 msd1 partner1 edu1 diab1 bp1 stroke1 chd1 cancer1 resp1 disab1 euro* cf* ep005_ ep329_
gen aika=4

append using share_red5, keep(mergeid int_year yrbirth country sex age1 msd1 partner1 edu1 diab1 bp1 stroke1 chd1 cancer1 resp1 disab1 euro* cf* nocompanion leftout isolated lonely ep005_ ep329_)
replace aika=5 if aika==.

append using share_red6, keep(mergeid int_year yrbirth country sex age1 msd1 partner1 edu1 diab1 bp1 stroke1 chd1 cancer1 resp1 disab1 euro* cf* nocompanion leftout isolated lonely ep005_ ep329_)
replace aika=6 if aika==.
	
append using share_red7, keep(mergeid int_year yrbirth country sex age1 msd1 partner1 edu1 diab1 bp1 stroke1 chd1 cancer1 resp1 disab1 euro* cf* nocompanion leftout isolated lonely ep005_ ep329_)
replace aika=7 if aika==.

append using share_red8, keep(mergeid int_year yrbirth country sex age1 msd1 partner1 edu1 diab1 bp1 stroke1 chd1 cancer1 resp1 disab1 euro* cf* nocompanion leftout isolated lonely ep005_ ep329_)
replace aika=8 if aika==.


// Depression
foreach v in 1 2 3 4 5 6 7 8 9 10 11 12 {
	recode euro`v' (-2/-1=.), gen(dep`v')
	tab euro`v' dep`v', nolab
	}
	
gen depsum = dep1+dep2+dep3+dep4+dep5+dep6+dep7+dep8+dep9+dep10+dep11+dep12	

drop euro*

gen disease_sum= diab1 + bp1 + stroke1 + chd1 + cancer1 + resp1 

gen int_year1=int_year
gen yrbirth1=yrbirth

drop int_year yrbirth

egen newid = group(mergeid)
sort newid aika
xtset newid aika

gen country1=country

order mergeid newid aika country cf*

// Cognition

// k003–k006
forval i=3/6 {
	tab cf00`i'_
	tab cf00`i'_, nolab
	}

forval i=3/6 {
	recode cf00`i'_ (-9/-1=.) (2=0) (1=1), gen(k00`i')
	tab k00`i'
	}	
		
// k104–107
forval i=4/7 {
	tab cf10`i'tot
	tab cf10`i'tot, nolab
	gen k10`i'=cf10`i'tot
	tab k10`i'
	}
	
// k010
tab cf010_
tab cf010_, nolab

recode cf010_ (-2/-1=.), gen(k010)
tab k010

// k012
tab cf012_
tab cf012_, nolab

recode cf012_ (-2/-1=.) (1=1) (2/97=0), gen(k012)
tab k012

// k014
tab cf014_
tab cf014_, nolab

recode cf014_ (-2/-1=.) (1=1) (2/97=0), gen(k014)
tab k014

// k015
tab cf015_
tab cf015_, nolab

recode cf015_ (-2/-1=.) (1=1) (2/97=0), gen(k015)
tab k015

// k108–k112
forval i=108/112 {
	tab cf`i'
	tab cf`i', nolab
	}

recode cf108_ (-2/-1=.) (min/-3=0) (0/92=0) (93=1) (94/max=0), gen(k108)
tab cf108_, miss
tab k108, miss

recode cf109_ (-2/-1=.) (min/-3=0) (0/85=0) (86=1) (87/max=0), gen(k109)
tab cf109_, miss
tab k109, miss

recode cf110_ (-2/-1=.) (min/-3=0) (0/78=0) (79=1) (80/max=0), gen(k110)
tab cf110_, miss
tab k110, miss

recode cf111_ (-2/-1=.) (min/-3=0) (0/71=0) (72=1) (73/max=0), gen(k111)
tab cf111_, miss
tab k111, miss

recode cf112_ (-2/-1=.) (min/-3=0) (0/64=0) (65=1) (66/max=0), gen(k112)
tab cf112_, miss
tab k112, miss

// k113–116
forval i=3/6 {
	tab cf11`i'tot
	tab cf11`i'tot, nolab
	gen k11`i'=cf11`i'tot
	tab k11`i'
	}

// k008
tab cf008tot
gen k008=cf008tot

// k016
tab cf016tot
gen k016=cf016tot
	
// k827–829
forval i=827/829 {
	tab cf`i'_
	tab cf`i'_, nolab
	recode cf`i'_ (-2/-1=.) (1=1) (5=0), gen(k`i')
	tab k`i'
	
	}

sort newid aika
xtset newid aika


save share_variables, replace
