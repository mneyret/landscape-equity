arable_weeds = data.table(read_excel('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Arable_weeds.xlsx', sheet = 2))
colnames(arable_weeds)[3] = 'Frequency'

all_sp = unique( c(Abundance_forests$Species,Abundance_grasslands$Species))

#### Deal with taxonomy ####
Species_correct_table = tpl::tpl.get(arable_weeds$Species,return.synonyms = T)
arable_weeds[, Species_correct := gsub(' ', '_', Species_correct_table$all.entries$name)]
arable_weeds[Species == 'Capsella bursa-pastoris', Species_correct := 'Capsella_bursa_pastoris']
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
arable_weeds[!(Species_correct %in% all_sp), Species_correct := get_synonyms(Species), by =ID]

missing_spe = sort(arable_weeds[!(Species_correct %in% all_sp), Species_correct])
sa = sapply(missing_spe, function(x){
  gen = unlist(strsplit(x, ' '))[1]
  return(all_sp[grepl(gen, all_sp)])
})
get_synonyms = function(x){
    print(x)
    syno = gsub(' ', '_', unique(Species_correct_table$synonyms[Species_correct_table$synonyms$original.search == x, ]$name.synonym)) 
    syno = syno[syno %in% all_sp]
    if (length(syno) >0){
      return(syno)
    }
    else {return(x)}
  }
}

all_sp[grepl('Medicago', all_sp)]
tpl::tpl.get('Centaurea jacea', return.synonyms = T)
tpl::tpl.get(arable_weeds$Species,return.synonyms = T)
Species_correct$synonyms[(gsub(' ', '_', Species_correct$synonyms$name.synonym) %in% all_sp) & 
                                      !(gsub(' ', '_', Species_correct$synonyms$name.accepted) %in% all_sp),]
arable_weeds[is.na(Species_correct), Species_correct := gsub(' ', '_', Species)]

# Check coverage
arable_weeds[(gsub(' ', '_', arable_weeds$Species_correct) %in% all_sp), sum(Frequency)]/arable_weeds[, sum(Frequency)]

fwrite(arable_weeds, '/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Arable_weeds_to_use.txt')


}
