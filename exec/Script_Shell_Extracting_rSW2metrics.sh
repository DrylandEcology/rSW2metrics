#!/bin/bash

prjoptions="-add_aggs_across_yrs -fparam=Project_Parameters.R"
mode="-mode=full"
parallel="-ncores=2 -cllog=FALSE"


# Extract soil inputs to simulations
Rscript Script_to_Extract_Metric.R -o=input_soillayers_depth -fun=collect_input_soillayers_depth ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=input_soillayers_N -fun=collect_input_soillayers_count ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=input_soillayers_sand -fun=collect_input_soillayers_sand ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=input_soillayers_clay -fun=collect_input_soillayers_clay ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=input_soillayers_gravel -fun=collect_input_soillayers_gravel ${mode} ${parallel} ${prjoptions}


# Extract metrics
Rscript Script_to_Extract_Metric.R -o=AI_annual -fun=metric_AI ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Climate_annual -fun=metric_Climate_annual ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Climate_monthly -fun=metric_Climate_monthly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Climate_quarterly -fun=metric_Climate_quarterly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=CorTempPPT_annual -fun=metric_CorTempPPT ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=CorTP_annual -fun=metric_CorTP_Annual ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=CWD_annual -fun=metric_CWD ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DDDat5C0to020cm30bar_quarterly -fun=metric_DDDat5C0to020cm30bar_quarterly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DDDat5C0to030cm30bar_annual -fun=metric_DDDat5C0to030cm30bar ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DDDat5C0to100cm30bar_annual -fun=metric_DDDat5C0to100cm30bar ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DDDat5C0to100cm30bar_quarterly -fun=metric_DDDat5C0to100cm30bar_quarterly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DR_annual -fun=metric_DR_annual ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DR_daily -fun=metric_DR_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DR_monthly -fun=metric_DR_monthly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DR_JJA -fun=metric_DR_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DrySoilDays_Seasonal_top50cm -fun=metric_DrySoilDays_Seasonal_top50cm ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DrySoilDays_Seasonal_wholeprofile -fun=metric_DrySoilDays_Seasonal_wholeprofile ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DSIat0to100cm15bar_annual -fun=metric_DSIat0to100cm15bar ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=DSIat0to100cm30bar_annual -fun=metric_DSIat0to100cm30bar ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=ET_annual -fun=metric_ET_annual ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=ET_monthly -fun=metric_ET_monthly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Evaporation_Seasonal -fun=metric_Evaporation_Seasonal ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=ExtremeShortTermDryStress_Seasonal_top50cm -fun=metric_ExtremeShortTermDryStress_Seasonal_top50cm ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=ExtremeShortTermDryStress_Seasonal_wholeprofile -fun=metric_ExtremeShortTermDryStress_Seasonal_wholeprofile ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=FrostDays_Seasonal -fun=metric_FrostDays_Seasonal ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=FrostDaysAtNeg5C_annual -fun=metric_FrostDaysAtNeg5C ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=LandCover_annualClim -fun=metric_land_cover_v2 ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=NonDrySWA_Seasonal_top50cm -fun=metric_NonDrySWA_Seasonal_top50cm ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=NonDrySWA_Seasonal_wholeprofile -fun=metric_NonDrySWA_Seasonal_wholeprofile ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=PET_Seasonal -fun=metric_PET_Seasonal ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=PPT_daily -fun=metric_PPT_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=PPT_dailyClim -fun=metric_PPT_dailyClim ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=PPT_JJA -fun=metric_PPT_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=PPT_monthlyClim -fun=metric_PPT_monthlyClim ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=PPT_Seasonal -fun=metric_PPT_Seasonal ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Radiation_annual -fun=metric_Radiation_annual ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Radiation_monthly -fun=metric_Radiation_monthly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=RecruitmentIndex_v4 -fun=metric_RecruitmentIndex_v4 ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=RecruitmentIndex_v5 -fun=metric_RecruitmentIndex_v5 ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SemiDryDuration_Annual_top50cm -fun=metric_SemiDryDuration_Annual_top50cm ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SemiDryDuration_Annual_wholeprofile -fun=metric_SemiDryDuration_Annual_wholeprofile ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SMTRs_annualClim -fun=metric_SMTRs ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWA_Seasonal_top50cm -fun=metric_SWA_Seasonal_top50cm ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWA_Seasonal_wholeprofile -fun=metric_SWA_Seasonal_wholeprofile ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat0to020cm39bar_daily -fun=metric_SWAat0to020cm39bar_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat0to020cm39bar_dailyClim -fun=metric_SWAat0to020cm39bar_dailyClim ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat0to020cm39bar_JJA -fun=metric_SWAat0to020cm39bar_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat0to020cm39bar_JJA -fun=metric_SWAat0to020cm39bar_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat0to020to100cm39bar_quarterly -fun=metric_SWAat0to020to100cm39bar_quarterly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat0to100cm30bar_annual -fun=metric_SWAat0to100cm30bar ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat0to100cm39bar_annual -fun=metric_SWAat0to100cm39bar ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat0to100cm39bar_daily -fun=metric_SWAat0to100cm39bar_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat0to100cm39bar_JJA -fun=metric_SWAat0to100cm39bar_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat20to040cm39bar_daily -fun=metric_SWAat20to040cm39bar_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat20to040cm39bar_JJA -fun=metric_SWAat20to040cm39bar_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat20to100cm39bar_daily -fun=metric_SWAat20to100cm39bar_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat20to100cm39bar_dailyClim -fun=metric_SWAat20to100cm39bar_dailyClim ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat20to100cm39bar_JJA -fun=metric_SWAat20to100cm39bar_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat40to060cm39bar_daily -fun=metric_SWAat40to060cm39bar_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat40to060cm39bar_JJA -fun=metric_SWAat40to060cm39bar_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat60to080cm39bar_daily -fun=metric_SWAat60to080cm39bar_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat60to080cm39bar_JJA -fun=metric_SWAat60to080cm39bar_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat80to100cm39bar_daily -fun=metric_SWAat80to100cm39bar_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWAat80to100cm39bar_JJA -fun=metric_SWAat80to100cm39bar_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWPat0to020cm_dailyClim -fun=metric_SWPat0to020cm_dailyClim ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=SWPat20to100cm_dailyClim -fun=metric_SWPat20to100cm_dailyClim ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=TDDat5C_annual -fun=metric_TDDat5C ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=TDDat5C_quarterly -fun=metric_TDDat5C_quarterly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=TemperatureMax_Seasonal -fun=metric_TemperatureMax_Seasonal ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=TemperatureMean_Seasonal -fun=metric_TemperatureMean_Seasonal ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=TemperatureMin_Seasonal -fun=metric_TemperatureMin_Seasonal ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=TEPET_daily -fun=metric_TEPET_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Tmean_daily -fun=metric_Tmean_daily ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Tmean_dailyClim -fun=metric_Tmean_dailyClim ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Tmean_JJA -fun=metric_Tmean_JJA ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Tmean_monthly -fun=metric_Tmean_monthly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Tmean_monthlyClim -fun=metric_Tmean_monthlyClim ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=Transpiration_Seasonal -fun=metric_Transpiration_Seasonal ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=TranspirationSeasonality_v6 -fun=metric_TranspirationSeasonality_v6 ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=veg_biomass_annual -fun=metric_veg_biomass_annual_v2 ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=veg_biomass_monthly -fun=metric_veg_biomass_monthly_v2 ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=VWC_Seasonal_top50cm -fun=metric_VWC_Seasonal_top50cm ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=VWC_Seasonal_wholeprofile -fun=metric_VWC_Seasonal_wholeprofile ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=WDDat5C0to020cm15bar_quarterly -fun=metric_WDDat5C0to020cm15bar_quarterly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=WDDat5C0to100cm15bar_quarterly -fun=metric_WDDat5C0to100cm15bar_quarterly ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=WDDat5C0to100cm15bar_annual -fun=metric_WDDat5C0to100cm15bar ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=WetSoilDays_Seasonal_top50cm -fun=metric_WetSoilDays_Seasonal_top50cm ${mode} ${parallel} ${prjoptions}
Rscript Script_to_Extract_Metric.R -o=WetSoilDays_Seasonal_wholeprofile -fun=metric_WetSoilDays_Seasonal_wholeprofile ${mode} ${parallel} ${prjoptions}


# Prepare daily simulation output for sharing
Rscript Script_to_Extract_Metric.R -o=SW2toTable_daily -fun=metric_SW2toTable_daily ${mode} ${parallel} ${prjoptions}
