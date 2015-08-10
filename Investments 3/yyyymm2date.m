function matlabDate = yyyymm2date(yyyymm);

year = floor(yyyymm/100);
month = yyyymm-100*year;
date = 1;
matlabDate = datenum(year,month,repmat(date,length(year),1));