/*

Machine Learning Metaprogramming for R
Check that gbm gives the same values in R and SAS.
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


* This should match the number of trees in R.;
%let n_trees = 100;

* The maximum acceptable difference.;
%let max_diff = 1e-10;

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
	datafile="synthetic.csv"
	out=synthetic
	dbms=csv
	replace;
run;

* This is analogous to predict.gbm() in R.;
data synthetic2;
	set synthetic;
	%include "synthetic.sas" /nosource nosource2 lrecl=100000;
	sas_pred_all = gbm;
run;      

* Check that both the individual trees and final prediction match.;
%macro check;
data check;
	set synthetic2;
	%do i = 1 %to &n_trees;
		diff_pred_&i = abs(r_pred_&i - gbm&i);
		if diff_pred_&i > &max_diff then put 'ERROR: ' _N_= diff_pred_all=;
	%end;
	diff_pred_all = abs(sas_pred_all - r_pred_all);
	if diff_pred_all > &max_diff then put 'ERROR: ' _N_= diff_pred_all=;
run;
%mend;
%check;

* Show the largest differences first.;
proc sort data=check;
	by descending diff_pred_all ;
run;
