/*

Machine Learning Metaprogramming for R
Check that bagEarth gives the same values in R and SAS.
by Andrew Ziem
Copyright (c) 2014 Compassion International

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/


* The maximum acceptable difference.;
%let max_diff = 1e-12;

* Number of bootstrap samples (must match the value B in R).;
%let B = 10;

* Find the .csv and .sas files created in R.;
%macro setwd();
%if %sysget(TMP) eq %then %do;
x "cd /tmp";
%end;
%else %do;
x "cd %sysget(TMP)";
%end;
%mend;
%setwd;

* Import data from R.;
proc import
	datafile="mlmeta_bagEarth_reg.csv"
	out=bagEarth
	dbms=csv
	replace;
run;

* This is analogous to predict() in R.;
data bagEarth;
	set bagEarth;
	%include "mlmeta_bagEarth_reg.sas" / nosource nosource2 lrecl=100000;
run;      


* Check that both the individual earth and final prediction match.;
%macro check;
data bagEarth;
	set bagEarth;
	%do i = 1 %to &B;
		diff_pred_&i = abs(r_pred_&i - prediction_&i);
		if diff_pred_&i > &max_diff then put 'ERROR: ' _N_= diff_pred_&i=;
	%end;
	diff_pred_all = abs(prediction - r_pred_all);
	if diff_pred_all > &max_diff then put 'ERROR: ' _N_= diff_pred_all=;
run;
%mend;
%check;

* Show the largest differences first.;
proc sort data=bagEarth;
	by descending diff_pred_all;
run;
