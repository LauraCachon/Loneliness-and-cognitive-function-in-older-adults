clear all

set maxvar 15000

// wave 4
clear
cd "/Users/kjkomula/Documents/share_all_data/sharew4_rel7-0-0_ALL_datasets_stata"
use "sharew4_rel7-0-0_ac"
foreach w in /*ac*/ as br cf ch co cv_r dn dropoff ep ex /*ft*/ gs /*gv_exrates*/ gv_health gv_housing /*gv_imputations*/ gv_isced gv_networks gv_ssw gv_weights hc hh ho /*iv*/ mh pf ph sn sp /*technical_variables xt*/ {
di "`w'"
merge 1:1 mergeid using "sharew4_rel7-0-0_`w'", nogen
}
cd "/Users/kjkomula/Documents/share_all_data"
save share4, replace


// wave 5
clear
cd "/Users/kjkomula/Documents/share_all_data/sharew5_rel7-0-0_ALL_datasets_stata"
use "sharew5_rel7-0-0_ac"
foreach w in /*ac*/ as br cf ch co cs cv_r dn dropoff ep ex /*ft*/ gs gv_deprivation /*gv_exrates*/ gv_health gv_housing /*gv_imputations*/ gv_isced gv_weights hc hh ho /*interviewer_survey*/ it /*iv*/ mc mh ph sp /*technical_variables xt*/ {
di "`w'"
merge 1:1 mergeid using "sharew5_rel7-0-0_`w'", nogen
}
cd "/Users/kjkomula/Documents/share_all_data"
save share5, replace


// wave 6
clear
cd "/Users/kjkomula/Documents/share_all_data/sharew6_rel7-0-0_ALL_datasets_stata"
use "sharew6_rel7-0-0_ac"
foreach w in /*ac*/ as br bs cf ch co cv_r dn dropoff ep ex /*ft*/ gs gv_children gv_dbs /*gv_exrates*/ gv_health gv_housing /*gv_imputations*/ gv_isced gv_networks gv_weights hc hh ho /*interviewer_survey*/ it /*iv*/ mh pf ph sn sp /*technical_variables xt*/ {
di "`w'"
merge 1:1 mergeid using "sharew6_rel7-0-0_`w'", nogen
}
cd "/Users/kjkomula/Documents/share_all_data"
save share6, replace


// wave 7
clear
cd "/Users/kjkomula/Documents/share_all_data/sharew7_rel7-0-0_ALL_datasets_stata"
use "sharew7_rel7-0-0_ac"
foreach w in /*ac*/ as br /*cc*/ cf ch co cv_r dn dq dropoff ep ex /*fs ft gl */ gs gv_big5 gv_children /*gv_exrates*/ gv_health gv_housing /*gv_imputations*/ gv_isced gv_weights hc hh ho /*hs*/ it /*iv*/ mh ph /*ra rc re rh */ rp sp /*technical_variables wq xt*/ {
di "`w'"
merge 1:1 mergeid using "sharew7_rel7-0-0_`w'", nogen
}
cd "/Users/kjkomula/Documents/share_all_data"
save share7, replace


// wave 8
clear
cd "/Users/kjkomula/Documents/share_all_data/sharew8_rel1-0-0_ALL_datasets_stata"
use "sharew8_rel1-0-0_ac"
foreach w in /*ac*/ as ax br cf ch co cv_r dn dropoff ep ex /*ft*/ gs /* gv_accelerometer_day gv_accelerometer_hour gv_accelerometer_total */ gv_big5 gv_children /*gv_exrates*/ gv_health gv_isced gv_networks gv_weights hc hh ho it /*iv*/ mh ph sn sp /*sr te technical_variables xt*/ {
di "`w'"
merge 1:1 mergeid using "sharew8_rel1-0-0_`w'", nogen
}
cd "/Users/kjkomula/Documents/share_all_data"
save share8, replace


