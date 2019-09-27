#Crime Re-Labeling Script

library(stringr)
library(dplyr)


#Chicago

chicago_1 <- chicago_10_19_census[1:1000000,]
chicago_2 <- chicago_10_19_census[1000001:2000000,]
chicago_3 <- chicago_10_19_census[2000001:2873194,]



chicago_1_cleaned <- chicago_1 %>%
  dplyr::select(cmplnt_fr_dt,ofns_desc,prem_typ_desc,year,latitude,longitude,month,day,fips,city) %>%
  mutate(FBI.Label = NA) %>%
  mutate(FBI.Label = if_else(str_detect(ofns_desc, "homicide"),"criminal homicide",
                                             if_else(str_detect(ofns_desc,"robbery"),"robbery",
                                              if_else(str_detect(ofns_desc,fixed("aggravated assault")),
                                                                        "Aggravated Assault",
                                              if_else(str_detect(ofns_desc,"burglary"),"burglary",
                                              if_else(str_detect(ofns_desc,"theft"),"theft(including auto)","NA")))))) %>%
  filter(FBI.Label != "NA") %>%
  mutate(ofns_desc = FBI.Label) %>%
  dplyr::select(-FBI.Label)


chicago_2_cleaned <- chicago_2 %>%
  dplyr::select(cmplnt_fr_dt,ofns_desc,prem_typ_desc,year,latitude,longitude,month,day,fips,city) %>%
  mutate(FBI.Label = NA) %>%
  mutate(FBI.Label = if_else(str_detect(ofns_desc, "homicide"),"criminal homicide",
                             if_else(str_detect(ofns_desc,"robbery"),"robbery",
                                     if_else(str_detect(ofns_desc,fixed("aggravated assault")),
                                             "Aggravated Assault",
                                             if_else(str_detect(ofns_desc,"burglary"),"burglary",
                                                     if_else(str_detect(ofns_desc,"theft"),"theft(including auto)","NA")))))) %>%
  filter(FBI.Label != "NA") %>%
  mutate(ofns_desc = FBI.Label) %>%
  dplyr::select(-FBI.Label)


chicago_3_cleaned <- chicago_3 %>%
  dplyr::select(cmplnt_fr_dt,ofns_desc,prem_typ_desc,year,latitude,longitude,month,day,fips,city) %>%
  mutate(FBI.Label = NA) %>%
  mutate(FBI.Label = if_else(str_detect(ofns_desc, "homicide"),"criminal homicide",
                             if_else(str_detect(ofns_desc,"robbery"),"robbery",
                                     if_else(str_detect(ofns_desc,fixed("aggravated assault")),
                                             "Aggravated Assault",
                                             if_else(str_detect(ofns_desc,"burglary"),"burglary",
                                                     if_else(str_detect(ofns_desc,"theft"),"theft(including auto)","NA")))))) %>%
  filter(FBI.Label != "NA") %>%
  mutate(ofns_desc = FBI.Label) %>%
  dplyr::select(-FBI.Label)

chicago_cleaned <- rbind(chicago_1_cleaned,chicago_2_cleaned,chicago_3_cleaned)

write.csv(chicago_cleaned, file = "~/Desktop/chicago_cleaned.csv")




#NYC

NYC_1 <- nyc_10_18_census[1:1000000,]
NYC_2 <- nyc_10_18_census[1000001:2000000,]
NYC_3 <- nyc_10_18_census[2000001:3000000,]
NYC_4 <- nyc_10_18_census[3000001:4000000,]
NYC_5 <- nyc_10_18_census[4000001:4376412,]


NYC_1_cleaned <- NYC_1 %>%
  dplyr::select(cmplnt_fr_dt,ofns_desc,prem_typ_desc,year,latitude,longitude,month,day,fips,city) %>%
  mutate(FBI.Label = NA) %>%
  mutate(FBI.Label = if_else(str_detect(ofns_desc, "murder"),"criminal homicide",
                                     if_else(str_detect(ofns_desc,"robbery"),"robbery",
                                             if_else(str_detect(ofns_desc,fixed("felony assault")),
                                                     "aggravated assault",
                                                    if_else(str_detect(ofns_desc,"burglary"),"burglary",
                                                    if_else(str_detect(ofns_desc,"larceny"),"theft(including auto)","NA")))))) %>%
  filter(FBI.Label != "NA") %>%
  mutate(ofns_desc = FBI.Label) %>%
  dplyr::select(-FBI.Label)


NYC_2_cleaned <- NYC_2 %>%
  dplyr::select(cmplnt_fr_dt,ofns_desc,prem_typ_desc,year,latitude,longitude,month,day,fips,city) %>%
  mutate(FBI.Label = NA) %>%
  mutate(FBI.Label = if_else(str_detect(ofns_desc, "murder"),"criminal homicide",
                             if_else(str_detect(ofns_desc,"robbery"),"robbery",
                                     if_else(str_detect(ofns_desc,fixed("felony assault")),
                                             "aggravated assault",
                                             if_else(str_detect(ofns_desc,"burglary"),"burglary",
                                                     if_else(str_detect(ofns_desc,"larceny"),"theft(including auto)","NA")))))) %>%
  filter(FBI.Label != "NA") %>%
  mutate(ofns_desc = FBI.Label) %>%
  dplyr::select(-FBI.Label)


NYC_3_cleaned <- NYC_3 %>%
  dplyr::select(cmplnt_fr_dt,ofns_desc,prem_typ_desc,year,latitude,longitude,month,day,fips,city) %>%
  mutate(FBI.Label = NA) %>%
  mutate(FBI.Label = if_else(str_detect(ofns_desc, "murder"),"criminal homicide",
                             if_else(str_detect(ofns_desc,"robbery"),"robbery",
                                     if_else(str_detect(ofns_desc,fixed("felony assault")),
                                             "aggravated assault",
                                             if_else(str_detect(ofns_desc,"burglary"),"burglary",
                                                     if_else(str_detect(ofns_desc,"larceny"),"theft(including auto)","NA")))))) %>%
  filter(FBI.Label != "NA") %>%
  mutate(ofns_desc = FBI.Label) %>%
  dplyr::select(-FBI.Label)




NYC_4_cleaned <- NYC_4 %>%
  dplyr::select(cmplnt_fr_dt,ofns_desc,prem_typ_desc,year,latitude,longitude,month,day,fips,city) %>%
  mutate(FBI.Label = NA) %>%
  mutate(FBI.Label = if_else(str_detect(ofns_desc, "murder"),"criminal homicide",
                             if_else(str_detect(ofns_desc,"robbery"),"robbery",
                                     if_else(str_detect(ofns_desc,fixed("felony assault")),
                                             "aggravated assault",
                                             if_else(str_detect(ofns_desc,"burglary"),"burglary",
                                                     if_else(str_detect(ofns_desc,"larceny"),"theft(including auto)","NA")))))) %>%
  filter(FBI.Label != "NA") %>%
  mutate(ofns_desc = FBI.Label) %>%
  dplyr::select(-FBI.Label)



NYC_5_cleaned <- NYC_5 %>%
  dplyr::select(cmplnt_fr_dt,ofns_desc,prem_typ_desc,year,latitude,longitude,month,day,fips,city) %>%
  mutate(FBI.Label = NA) %>%
  mutate(FBI.Label = if_else(str_detect(ofns_desc, "murder"),"criminal homicide",
                             if_else(str_detect(ofns_desc,"robbery"),"robbery",
                                     if_else(str_detect(ofns_desc,fixed("felony assault")),
                                             "aggravated assault",
                                             if_else(str_detect(ofns_desc,"burglary"),"burglary",
                                                     if_else(str_detect(ofns_desc,"larceny"),"theft(including auto)","NA")))))) %>%
  filter(FBI.Label != "NA") %>%
  mutate(ofns_desc = FBI.Label) %>%
  dplyr::select(-FBI.Label)


nyc_cleaned <- rbind(NYC_1_cleaned,NYC_2_cleaned,NYC_3_cleaned,NYC_4_cleaned,NYC_5_cleaned)

write.csv(nyc_cleaned,file = "~/Desktop/nyc_cleaned.csv")

#Chapel Hill


Chapel_Hill_cleaned <- chapel_hill_10_17_census_chapel_hill_10_17_census %>%
  dplyr::select(cmplnt_fr_dt,ofns_desc,prem_typ_desc,year,latitude,longitude,month,day,fips,city) %>%
  mutate(FBI.Label = NA) %>%
  mutate(FBI.Label = if_else(str_detect(ofns_desc, "homicide"),"criminal homicide",
                             if_else(str_detect(ofns_desc,"murder"),"criminal homicide",
                              if_else(str_detect(ofns_desc,"robbery"),"robbery",
                              if_else(str_detect(ofns_desc,fixed("aggravated assault")),"aggravated assault",
                              if_else(str_detect(ofns_desc,"b&e"),"burglary",
                              if_else(str_detect(ofns_desc,"larceny"),"theft(including auto)",
                              if_else(str_detect(ofns_desc,"burglary"),"burglary",
                              if_else(str_detect(ofns_desc,"automobile theft"),"theft(including auto)",
                              if_else(str_detect(ofns_desc, "breaking and entering"),"burglary",
                              if_else(str_detect(ofns_desc, "other vehicle theft"),"theft(including auto)",
                              if_else(str_detect(ofns_desc,"assault with a"),"aggravated assault",
                              if_else(str_detect(ofns_desc, "assault inflict"),"aggravated assault","NA"))))))))))))) %>%
  filter(FBI.Label != "NA") %>%
  mutate(ofns_desc = FBI.Label) %>%
  dplyr::select(-FBI.Label)
  

write.csv(Chapel_Hill_cleaned, file = "~/Desktop/Chapel_Hill_Cleaned.csv")
