/*

Machine Learning Metaprogramming for R
Check that ctree gives the same values in R and SAS.
by Andrew Ziem
Copyright (c) 2014, 2015 Compassion International

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

%macro test_ctree(suffix);
* Import data from R.;
proc import
	datafile="mlmeta_ctree_&suffix..csv"
	out=ctree_&suffix.
	dbms=csv
	replace;
run;

* This is analogous to predict() in R.;
data ctree_&suffix.;
	set ctree_&suffix.;
	%include "mlmeta_ctree_&suffix..sas" / nosource nosource2 lrecl=100000;
run;
%if &syserr ne 0 %then %abort cancel;

data ctree_&suffix.;
	set ctree_&suffix.;
	if not missing(prediction) then do;
		difference_prediction = abs(prediction - pred);
		if difference_prediction > &max_diff then put 'ERROR: ' _N_= difference_prediction=;
		end;
	else
		put 'WARNING: missing prediction ' _N_;
	N = _N_;
run;
%if &syserr ne 0 %then %abort cancel;

* Show the largest differences first.;
proc sort data=ctree_&suffix.;
	by descending difference_prediction;
run;
%mend;

%test_ctree(reg);
%test_ctree(binary);

