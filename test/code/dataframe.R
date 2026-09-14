# primary energy ----------------------------------------------------------
prmene <- tribble(~Variable,~Legend,~Color,
                  'Prm_Ene_Oil_wo_CCS','Oil w/o CCS','sandybrown',
                  'Prm_Ene_Oil_w_CCS','Oil w/ CCS','tan3',
                  'Prm_Ene_Coa_wo_CCS','Coal w/o CCS','grey50',
                  'Prm_Ene_Coa_w_CCS','Coal w/ CCS','grey30',
                  'Prm_Ene_Gas_wo_CCS','Gas w/o CCS','lightgoldenrod',
                  'Prm_Ene_Gas_w_CCS','Gas w/ CCS','lightgoldenrod3',
                  'Prm_Ene_Nuc','Nuclear','moccasin',
                  'Prm_Ene_Hyp','Hydro','lightsteelblue',
                  'Prm_Ene_Bio_wo_CCS','Biomass w/o CCS','darkolivegreen2',
                  'Prm_Ene_Bio_w_CCS','Biomass w/ CCS','darkolivegreen4',
                  'Prm_Ene_Geo','Geothermal','peru',
                  'Prm_Ene_Solar','Solar','lightsalmon',
                  'Prm_Ene_Win','Wind','lightskyblue3',
                  'Prm_Ene_Oce','Ocean','paleturquoise3')


# Secondary energy --------------------------------------------------------
secele <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Ele_Oil_wo_CCS','Oil w/o CCS','sandybrown',
                  'Sec_Ene_Ele_Oil_w_CCS','Oil w/ CCS','tan3',
                  'Sec_Ene_Ele_Coa_wo_CCS','Coal w/o CCS','grey50',
                  'Sec_Ene_Ele_Coa_w_CCS','Coal w/ CCS','grey30',
                  'Sec_Ene_Ele_Gas_wo_CCS','Gas w/o CCS','lightgoldenrod',
                  'Sec_Ene_Ele_Gas_w_CCS','Gas w/ CCS','lightgoldenrod3',
                  'Sec_Ene_Ele_Nuc','Nuclear','moccasin',
                  'Sec_Ene_Ele_Hyp','Hydro','lightsteelblue',
                  'Sec_Ene_Ele_Bio_wo_CCS','Biomass w/o CCS','darkolivegreen2',
                  'Sec_Ene_Ele_Bio_w_CCS','Biomass w/ CCS','darkolivegreen4',
                  'Sec_Ene_Ele_Geo','Geothermal','peru',
                  'Sec_Ene_Ele_Solar','Solar','lightsalmon',
                  'Sec_Ene_Ele_Win','Wind','lightskyblue3',
                  'Sec_Ene_Ele_Hyd','Hydrogen','thistle2')
 

sechyd <- tribble(~Variable,~Legend,~Color,
                  'Sec_Ene_Hyd_Coa_wo_CCS','Coal w/o CCS','grey50',
                  'Sec_Ene_Hyd_Coa_w_CCS','Coal w/ CCS','grey30',
                  'Sec_Ene_Hyd_Gas_wo_CCS','Gas w/o CCS','lightgoldenrod',
                  'Sec_Ene_Hyd_Gas_w_CCS','Gas w/ CCS','lightgoldenrod3',
                  'Sec_Ene_Hyd_Bio_wo_CCS','Biomass w/o CCS','darkolivegreen2',
                  'Sec_Ene_Hyd_Bio_w_CCS','Biomass w/ CCS','darkolivegreen4',
                  'Sec_Ene_Hyd_Ele','Electricity','lightsteelblue')

# final energy ------------------------------------------------------------
finene <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Gas','Gas','moccasin',
                  'Fin_Ene_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Solar','Solar','lightsalmon',
                  'Fin_Ene_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Heat','Heat','salmon',
                  'Fin_Ene_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Industry
finind <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Ind_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Ind_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Ind_Gas','Gas','moccasin',
                  'Fin_Ene_Ind_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Ind_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Ind_Solar','Solar','lightsalmon',
                  'Fin_Ene_Ind_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Ind_Heat','Heat','salmon',
                  'Fin_Ene_Ind_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Ind_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Industry, Steel
finste <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Ind_Ste_Liq','Oil','sandybrown',
                  'Fin_Ene_Ind_Ste_SolidsFos','Coal','grey70',
                  'Fin_Ene_Ind_Ste_Gas','Gas','moccasin',
                  'Fin_Ene_Ind_Ste_SolidsBioEne','Biomass','#A9D65D',
                  'Fin_Ene_Ind_Ste_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Ind_Ste_Heat','Heat','salmon',
                  'Fin_Ene_Ind_Ste_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Ind_Ste_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Industry, Cement
fincem <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Ind_Cem_Liq','Oil','sandybrown',
                  'Fin_Ene_Ind_Cem_SolidsFos','Coal','grey70',
                  'Fin_Ene_Ind_Cem_Gas','Gas','moccasin',
                  'Fin_Ene_Ind_Cem_SolidsBioEne','Biomass','#A9D65D',
                  'Fin_Ene_Ind_Cem_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Ind_Cem_Heat','Heat','salmon',
                  'Fin_Ene_Ind_Cem_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Ind_Cem_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Industry, excl. Steel and Cement
finoth <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Ind_exc_Ste_and_Cem_Liq','Oil','sandybrown',
                  'Fin_Ene_Ind_exc_Ste_and_Cem_SolidsFos','Coal','grey70',
                  'Fin_Ene_Ind_exc_Ste_and_Cem_Gas','Gas','moccasin',
                  'Fin_Ene_Ind_exc_Ste_and_Cem_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Ind_exc_Ste_and_Cem_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Ind_exc_Ste_and_Cem_Heat','Heat','salmon',
                  'Fin_Ene_Ind_exc_Ste_and_Cem_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Ind_exc_Ste_and_Cem_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Buildings
finbui <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Res_and_Com_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Res_and_Com_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Res_and_Com_Gas','Gas','moccasin',
                  'Fin_Ene_Res_and_Com_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Res_and_Com_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Res_and_Com_Solar','Solar','lightsalmon',
                  'Fin_Ene_Res_and_Com_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Res_and_Com_Heat','Heat','salmon',
                  'Fin_Ene_Res_and_Com_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Res_and_Com_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Residential
finres <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Res_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Res_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Res_Gas','Gas','moccasin',
                  'Fin_Ene_Res_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Res_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Res_Solar','Solar','lightsalmon',
                  'Fin_Ene_Res_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Res_Heat','Heat','salmon',
                  'Fin_Ene_Res_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Res_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Commercial
fincom <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Com_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Com_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Com_Gas','Gas','moccasin',
                  'Fin_Ene_Com_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Com_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Com_Solar','Solar','lightsalmon',
                  'Fin_Ene_Com_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Com_Heat','Heat','salmon',
                  'Fin_Ene_Com_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Com_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Transport
fintra <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Tra_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Tra_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Tra_Gas','Gas','moccasin',
                  'Fin_Ene_Tra_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Tra_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Tra_Solar','Solar','lightsalmon',
                  'Fin_Ene_Tra_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Tra_Heat','Heat','salmon',
                  'Fin_Ene_Tra_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Tra_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Passenger transport
finpss <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Tra_Pss_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Tra_Pss_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Tra_Pss_Gas','Gas','moccasin',
                  'Fin_Ene_Tra_Pss_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Tra_Pss_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Tra_Pss_Solar','Solar','lightsalmon',
                  'Fin_Ene_Tra_Pss_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Tra_Pss_Heat','Heat','salmon',
                  'Fin_Ene_Tra_Pss_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Tra_Pss_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Freight transport
finfre <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Tra_Fre_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Tra_Fre_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Tra_Fre_Gas','Gas','moccasin',
                  'Fin_Ene_Tra_Fre_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Tra_Fre_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Tra_Fre_Solar','Solar','lightsalmon',
                  'Fin_Ene_Tra_Fre_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Tra_Fre_Heat','Heat','salmon',
                  'Fin_Ene_Tra_Fre_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Tra_Fre_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Road transport
finroa <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Tra_Roa_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Tra_Roa_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Tra_Roa_Gas','Gas','moccasin',
                  'Fin_Ene_Tra_Roa_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Tra_Roa_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Tra_Roa_Solar','Solar','lightsalmon',
                  'Fin_Ene_Tra_Roa_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Tra_Roa_Heat','Heat','salmon',
                  'Fin_Ene_Tra_Roa_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Tra_Roa_Liq_Hyd_syn','Synfuel','mediumorchid1')


# final energy- Aviation
finavi <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Tra_Avi_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Tra_Avi_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Tra_Avi_Gas','Gas','moccasin',
                  'Fin_Ene_Tra_Avi_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Tra_Avi_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Tra_Avi_Solar','Solar','lightsalmon',
                  'Fin_Ene_Tra_Avi_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Tra_Avi_Heat','Heat','salmon',
                  'Fin_Ene_Tra_Avi_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Tra_Avi_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Shipping
finshi <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Tra_Shi_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Tra_Shi_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Tra_Shi_Liq_Nat_Gas','Gas','moccasin',
                  'Fin_Ene_Tra_Shi_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Tra_Shi_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Tra_Shi_Solar','Solar','lightsalmon',
                  'Fin_Ene_Tra_Shi_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Tra_Shi_Heat','Heat','salmon',
                  'Fin_Ene_Tra_Shi_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Tra_Shi_Liq_Hyd_syn','Synfuel','mediumorchid1')

# final energy- Rail
finrai <- tribble(~Variable,~Legend,~Color,
                  'Fin_Ene_Tra_Rai_Liq_Oil','Oil','sandybrown',
                  'Fin_Ene_Tra_Rai_SolidsCoa','Coal','grey70',
                  'Fin_Ene_Tra_Rai_Gas','Gas','moccasin',
                  'Fin_Ene_Tra_Rai_SolidsBio','Biomass','#A9D65D',
                  'Fin_Ene_Tra_Rai_Liq_Bio','Biofuel','#DBFF70',
                  'Fin_Ene_Tra_Rai_Solar','Solar','lightsalmon',
                  'Fin_Ene_Tra_Rai_Ele','Electricity','lightsteelblue',
                  'Fin_Ene_Tra_Rai_Heat','Heat','salmon',
                  'Fin_Ene_Tra_Rai_Hyd','Hydrogen','thistle2',
                  'Fin_Ene_Tra_Rai_Liq_Hyd_syn','Synfuel','mediumorchid1')


# Emissions ---------------------------------------------------------------

# Sectoral emission
emisec <- tribble(~Variable,~Legend,~Color,
                  'Emi_CO2_Ene_Sup','Energy\nsupply','moccasin',
                  'Emi_CO2_Ene_Dem_Ind','Industry','salmon',
                  'Emi_CO2_Ene_Dem_Res_and_Com','Buildings','lightskyblue3',
                  'Emi_CO2_Ene_Dem_Tra','Transport','darkolivegreen2',
                  'Emi_CO2_Ene_Dem_Oth_Sec','Other\ndemand','grey',
#                  'Emi_CO2_Ene_Dem_AFO','AFOFI','paleturquoise3',
                  'Emi_CO2_Ind_Pro','Industrial\nprocess','sandybrown',
                  'Emi_CO2_Oth','DACCS','thistle2')

# Carbon capture
carcap <- tribble(~Variable,~Legend,~Color,
                  'Car_Cap_Ind_Pro','Industrialprocess','grey',
                  'Car_Cap_Fos_Ene_Sup','Energy  supply','moccasin',
                  'Car_Cap_Fos_Ene_Dem_Ind','Industry','salmon',
                  'Car_Cap_Bio_Ene_Sup','Bioenergy','darkolivegreen2',
                  'Car_Cap_Dir_Air_Cap','DAC','thistle2')

# Carbon sequestration
carseq <- tribble(~Variable,~Legend,~Color,
                  "Car_Seq_Geo_Sto_Dir_Air_Cap",'DACCS','thistle2',
                  "Car_Seq_Geo_Sto_Bio",'BECCS','darkolivegreen2',
                  "Car_Seq_Geo_Sto_Fos",'Fossil','sandybrown',
                  "Car_Seq_Geo_Sto_Oth",'Industrial process','grey60')


# Costs -------------------------------------------------------------------

# Additional investment
invadd <- tribble(~Variable,~Legend,~Color,
                  'Inv_Add_Ene_Dem_Ind','Industry','salmon',
                  'Inv_Add_Ene_Dem_Res_and_Com','buildings','lightsteelblue',
                  'Inv_Add_Ene_Dem_Tra','Transport','darkolivegreen2',
                  'Inv_Add_Ene_Sup','Energy supply','moccasin')


# Investment --------------------------------------------------------------
# Energy supply Electricity
invaddele <-  tribble(~Variable,~Legend,~Color,
                   'Inv_Add_Ene_Sup_Ele_Oil_wo_CCS','Oil w/o CCS','sandybrown',
                   'Inv_Add_Ene_Sup_Ele_Oil_w_CCS','Oil w/ CCS','tan3',
                   'Inv_Add_Ene_Sup_Ele_Coa_wo_CCS','Coal w/o CCS','grey50',
                   'Inv_Add_Ene_Sup_Ele_Coa_w_CCS','Coal w/ CCS','grey30',
                   'Inv_Add_Ene_Sup_Ele_Gas_wo_CCS','Gas w/o CCS','lightgoldenrod',
                   'Inv_Add_Ene_Sup_Ele_Gas_w_CCS','Gas w/ CCS','lightgoldenrod3',
                   'Inv_Add_Ene_Sup_Ele_Hyp','Hydro','lightsteelblue',
                   'Inv_Add_Ene_Sup_Ele_Nuc','Nuclear','moccasin',
                   'Inv_Add_Ene_Sup_Ele_Bio_wo_CCS','Biomass w/o CCS','darkolivegreen2',
                   'Inv_Add_Ene_Sup_Ele_Bio_w_CCS','Biomass w/ CCS','darkolivegreen4',
                   'Inv_Add_Ene_Sup_Ele_Geo','Geothermal','peru',
                   'Inv_Add_Ene_Sup_Ele_Solar','Solar','lightsalmon',
                   'Inv_Add_Ene_Sup_Ele_Win','Wind','lightskyblue3')


#  Electric vehicle --------------------------------------------------------
EVshapss <- tribble(~Variable,~Legend,~Color,
                  'Tec_sto_Sha_Tra_Pss_Roa_FCV','FCEV','plum3',
                  'Tec_sto_Sha_Tra_Pss_Roa_BEV','BEV','#b2e389',
                  'Tec_sto_Sha_Tra_Pss_Roa_PHV','PHV','lightpink',
                  'Tec_sto_Sha_Tra_Pss_Roa_HEV','Hybrid vehicle','#738ac8',
                  'Tec_sto_Sha_Tra_Pss_Roa_ICE','Conventional vehicle','#fed46c')

EVshafre <- tribble(~Variable,~Legend,~Color,
                    'Tec_sto_Sha_Tra_Fre_Roa_FCV','FCEV','plum3',
                    'Tec_sto_Sha_Tra_Fre_Roa_BEV','BEV','#b2e389',
                    'Tec_sto_Sha_Tra_Fre_Roa_PHV','PHV','lightpink',
                    'Tec_sto_Sha_Tra_Fre_Roa_HEV','Hybrid vehicle','#738ac8',
                    'Tec_sto_Sha_Tra_Fre_Roa_ICE','Conventional vehicle','#fed46c')