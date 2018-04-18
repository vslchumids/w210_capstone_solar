# SolaRise Optimization Model API

##URL

http://solarise.7bsuxv7th2.us-west-2.elasticbeanstalk.com/solarise?

##Params:

	station: [required] Weather Station (derived from Address in R Shiny layer)

	biz: [required] Business Type (please remove spaces between words)

	roof: [required] Roof Size (Sq ft)

	wkrs: [required] # Employees

	sqft: [required] office Size (Sq ft)

	wd_st: Weekday Start Hours

	wd_hrs: Weekday Daily Hours

	we_st: Weekend Start Hours

	we_hrs: Weekend Daily Hours

	open_24: Open 24 (=Y if checked; omitted if unchecked)

	elec_heat: Electric Heat (=Y if checked; omitted if unchecked)

	elec_cool: Electric Cool (=Y if checked; omitted if unchecked)

	open_wkend: Open Weekend (=Y if checked; omitted if unchecked)

	elec_cook: Eectric Cook (=Y if checked; omitted if unchecked)

	refridg: Refridgeration (=Y if checked; omitted if unchecked)

	elec_mfg: Electic Manufacture (=Y if checked; omitted if unchecked)