######## Plants ##########
library(readxl)
library(dplyr)
# Old version

arable_weeds = data.table(read_excel('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Arable_weeds.xlsx', sheet = 2))
colnames(arable_weeds)[3] = 'Frequency'

all_sp = unique( c(Abundance_forests$Species,Abundance_grasslands$Species))

#### Deal with taxonomy ####
 #Species_correct_table = tpl::tpl.get(arable_weeds$Species,return.synonyms = T)
 arable_weeds[, Species_correct := gsub(' ', '_', Species)]
 arable_weeds[Species == 'Capsella bursa-pastoris', Species_correct := 'Capsella_bursa_pastoris']
 arable_weeds[Species == 'Tripleurospermum inodorum', Species_correct := 'Tripleurospermum_perforatum']
 Tripleurospermum_perforatum
 arable_weeds[Species == 'Taraxacum officinale', Species_correct := "Taraxacum_sp"]
 arable_weeds[Species == 'Brassica napus', Species_correct := "Brassica_sp"]
 arable_weeds[Species == 'Hordeum vulgare', Species_correct := "Hordeum_vulgare_aggr."]
 arable_weeds[Species == "Crepis capillaris", Species_correct := "Crepis_sp"]
 arable_weeds[Species %in% c("Bromus commutatus", "Bromus horderaceus"), Species_correct := "Bromus_hordeaceus_aggr.incl_B_commutatus"]
 arable_weeds[Species == 'Poa pratensis', Species_correct := "Poa_pratensis_aggr."]
 arable_weeds[Species == "Medicago x varia", Species_correct := "Medicago_x_varia" ]
 arable_weeds[Species == "Persicaria lapathifolia", Species_correct := "Persicaria_lapathifolium" ]
 arable_weeds[Species == 'Vicia sativa', Species_correct := "Vicia_sativa_aggr."]
 arable_weeds$ID =  1:nrow(arable_weeds)
# arable_weeds[!(Species_correct %in% all_sp), Species_correct := get_synonyms(Species), by =ID]
# 
 missing_spe = sort(arable_weeds[!(Species_correct %in% all_sp), Species_correct])
 sa = sapply(missing_spe, function(x){
   gen = unlist(strsplit(x, ' '))[1]
   return(all_sp[grepl(gen, all_sp)])
 })
# get_synonyms = function(x){
#     print(x)
#     syno = gsub(' ', '_', unique(Species_correct_table$synonyms[Species_correct_table$synonyms$original.search == x, ]$name.synonym)) 
#     syno = syno[syno %in% all_sp]
#     if (length(syno) >0){
#       return(syno)
#     }
#     else {return(x)}
#   }
# }
# 
# Species_correct$synonyms[(gsub(' ', '_', Species_correct$synonyms$name.synonym) %in% all_sp) & 
#                                       !(gsub(' ', '_', Species_correct$synonyms$name.accepted) %in% all_sp),]
# arable_weeds[is.na(Species_correct), Species_correct := gsub(' ', '_', Species)]
# 
# # Check coverage
 arable_weeds[(gsub(' ', '_', arable_weeds$Species_correct) %in% all_sp), sum(Frequency)]/arable_weeds[, sum(Frequency)]
 
# fwrite(arable_weeds, '/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Arable_weeds_to_use.txt')
#fread('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Arable_weeds_to_use.txt')

# New version
NVC = data.table(read_excel('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/NVC-floristic-tables.xls'))
#OV3 in Sch and Hai cereals
#OV8 in Hai and Alb, cereals
#OV9 in any field, Hai and Alb
#OV1 oats barley in Hai/Alb
#OV13 Stellaria - Capsella

colnames(NVC) = gsub(' ', '_', colnames(NVC))
NVC = NVC[grepl('OV', Community_level_code),]
NVC[, Frequency := recode(Species_constancy_value, 'I' = 10, 'II' = 30, 'III' = 50, 'IV' = 70, 'V' = 90)]
NVC[, Species := gsub(' ', '_', Species_name_or_special_variable)]
NVC[, Cover := recode(Maximum_abundance_of_species, '10' = 96,
                      '9' = 83,
                      '8' = 63,
                      '7' = 42,
                      '6' = 30,
                      '5' = 18,
                      '4' = 7,
                      '3' = 3,
                      '2' = 2,
                      '1' = 1)]

# Check species names match
NVC[, Species_in_BE := Species %in% BEspe]
#NVC[Species_in_BE == TRUE, Species_correct := Species]
NVC[, found_in_other_german_data := (Species_name_or_special_variable %in% arable_weeds$Species)]
  
NVC_sp = sort(unique(NVC$Species))
BEspe = unique(Abundance_grasslands_forests01$Species)


NVC_sp[!(NVC_sp %in% BEspe)]
NVC[, 
    Species_correct := recode(Species,
                              "Aphanes_microcarpa"          = '' , #possible
                              "Matricaria_perforata"        = 'Tripleurospermum_perforatum', 
                              "Stellaria_media_agg."      =  'Stellaria_media' ,
                              "Bilderdykia_convolvulus"     = 'Fallopia_convolvulus' ,
                              "Chamomilla_suaveolens"       = 'Matricaria_discoidea', 
                              "Anthoxanthum_aristatum"    = '' ,#possible
                              "Viola_tricolor"              = 'Viola_sp' ,
                              "Scleranthus_annuus"          = '', #possible
                              "Bromus_hordeaceus"  =  'Bromus_hordeaceus_aggr.incl_B_commutatus',
                              "Chrysanthemum_segetum"     = '' ,
                              "Alopecurus_myosuroides"     = '' ,
                              "Bryum_erythrocarpum"        = '',  
                              "Dicranella_staphylina"     = '', 
                              "Ceratodon_purpureus"        = 'Ceratodon_purpureus_ssp_purpureus' ,
                              "Phascum_cuspidatum"         = '',  
                              "Anthemis_cotula"           = '', #??
                              "Riccia_sorocarpa"           = '' ,
                              "Papaver_argemone"           = '',  
                              "Capsella_bursa-pastoris"   = 'Capsella_bursa_pastoris', 
                              "Papaver_dubium"             = '' ,
                              "Arenaria_leptoclados"       = 'Arenaria_serpyllifolia',  
                              "Silene_alba"               = '', 
                              "Artemisia_vulgaris"         = '' ,
                              "Vicia_sativa"               = 'Vicia_sativa_aggr.', 
                              "Taraxacum_officinale"      = 'Taraxacum_sp', 
                              "Anthemis_arvensis"          = '' ,#??
                              "Veronica_triphyllos"        = '',  #v. verna?
                              "Pulicaria_dysenterica"     = '', 
                              "Descurainia_sophia"         = '' , #??
                              "Avena_fatua"                = '', 
                              "Polygonum_persicaria"      = 'Persicaria_maculosa', 
                              "Chenopodium_hybridum"       = 'Chenopodium_sp' ,
                              "Solanum_nigrum"             = '', 
                              "Urtica_urens"              = '', 
                              "Chamomilla_recutita"        = 'Matricaria_recutita' ,
                              "Coronopus_squamatus"        = '', 
                              "Euphorbia_peplus"          = '', 
                              "Aethusa_cynapium"           = '' ,
                              "Atriplex_patula"            = '',  #?Atriplex patula 
                              "Atriplex_prostrata"        = '', #?Atriplex philippii
                              "Matricaria_maritima"        = '' ,
                              "Hordeum_vulgare"            = 'Hordeum_vulgare_aggr.', 
                              "Chamerion_angustifolium"   = 'Epilobium_angustifolium', 
                              "Cerastium_fontanum"         = '' ,
                              "Juncus_bufonius_sens.lat."  = '', 
                              "Ranunculus_arvensis"       = 'Ranunculus_sp', 
                              "Crepis_capillaris"          = 'Crepis_sp' ,
                              "Mercurialis_annua"          = '', 
                              "Misopates_orontium"        = '', 
                              "Lolium_perenne_subsp.perenne" = 'Lolium_perenne',
                              "Filaginella_uliginosa"      = '',  #??
                              "Lamium_hybridum"           = 'Lamium_purpureum', 
                              "Polygonum_arenastrum"       = '' ,
                              "Pottia_intermedia"          = '', 
                              "Fumaria_muralis"           = 'Fumaria_officinalis', #Fumaria officinalis L.
                              "Veronica_agrestis"          = '' ,
                              "Chenopodium_urbicum"        = 'Chenopodium_sp', 
                              "Polygonum_nodosum"         = 'Persicaria_lapathifolium', 
                              "Fumaria_bastardii"          = '' ,
                              "Polygonum_lapathifolium"    = 'Persicaria_lapathifolium',  
                              "Brassica_napus"            = 'Brassica_sp', 
                              "Chenopodium_bonus-henricus" = 'Chenopodium_sp' ,
                              "Chenopodium_rubrum"         = '', #?
                              "Poa_pratensis_sens.lat."  = 'Poa_pratensis_aggr.'
                              )]


NVC[Species_correct == '', .N, by = Community_level_code]$N/NVC[, .N, by = Community_level_code]$N




arable_weeds[!(Species_correct %in% c(NVC$Species_correct,NVC$Species)) & !(Species %in% c(NVC$Species_correct,NVC$Species)), ]


All_data = arable_weeds
for (i in 1:nrow(All_data)){
  sp = All_data[i, Species_correct]
  if (sp %in% NVC$Species_correct){
    ab = mean(NVC[Species_correct == sp | Species == sp, as.numeric(Cover)])
  } else {
    print(paste(sp, 'not found'))
    ab = 1
  }
    All_data[i, Cover := ab]
  }



# Not sure about that
NVC_subset = NVC[!(Species_correct == '' & found_in_other_german_data == F), c('Species','Species_correct', 'Community_level_code', 'Frequency', 'Cover')]
NVC_subset[Species_correct == '', Species_correct := Species ]

fwrite(All_data, '/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Arable_weeds_to_use.txt')


######## Birds ##########
# Extracted from: 
# Landscape associations of farmland bird diversity in Germany and Japan
# Keiko Sasakia,*, Stefan Hotesb, Taku Kadoyac, Akira Yoshiokad, Volkmar Woltersa
bird_species = list(
'Buteo buteo' = 'SF',
'Ciconia ciconia' = 'R',
'Corvus corone corone' = 'F',
'Passer montanus' = 'F',
'Carduelis carduelis' = 'F',
'Corvus corax' = 'SF',
'Milvus milvus' = 'SF',
'Sturnus vulgaris' = 'F',
'Turdus pilaris' = 'SF',
'Alauda arvensis' = 'F',
'Acrocephalus palustris' = 'F',
'Hirundo rustica' = 'SF',
'Anthus pratensis' = 'SF',
'Phasianus colchicus' = 'F'	,
'Crex crex' = 'SF', # ***
'Carduelis cannabina' = 'SF'	,
'Emberiza schoeniclus' = 'F', # ***
'Coturnix coturnix' = 'SF',
'Gallinago gallinago' = 'SF',
'Emberiza citrinella' = 'F',
'Motacilla flava' = 'SF',
'Lanius collurio' = 'SF',
'Saxicola rubetra' = 'SF',
'Emberiza calandra' = 'SF', # *** original: Miliaria calandra
'Vanellus vanellus' = 'SF',
'Passer domesticus' = 'F',
'Perdix perdix' = 'SF'	, # ***
'Sylvia communis' = 'F')

# GBIF
gbif_data = fread('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/GBIF_birds.csv')
#to cite https://doi.org/10.15468/dl.avxpm7
gbif_data_oservation = gbif_data[, list(.N, sum(individualCount, na.rm = T)), by = c('stateProvince', 'occurrenceStatus','species', 'recordedBy','locality', 'eventDate')]
gbif_data_arable = gbif_data_oservation[species %in% names(bird_species),]

number_of_observation_points = nrow(unique(gbif_data_arable[, c('recordedBy', 'locality', 'eventDate')]))

occurence = gbif_data_arable[, sum(N)/number_of_observation_points, by = 'species' ]




Crop_birds = data.table('Species' = names(bird_species), 'Occurence_quali' = as.character(bird_species))

Crop_birds[, Frequency := dplyr::recode(Occurence_quali, 'SF' = 70, 'F' = 30, 'R' = 10) ]
fwrite(Crop_birds, '/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Arable_birds_to_use.txt')

