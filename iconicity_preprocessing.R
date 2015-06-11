# Iconicity preprocessing
### Bodo Wintr
### June 10, 2015

### Preliminaries:

setwd("/Users/teeniematlock/Desktop/research/iconicity/analysis/")
icon1 <- read.csv("english_iconicity_first_norming_bout.csv")
icon2 <- read.csv("english_iconicity_second_norming_bout.csv")
library(dplyr)

### Take averages:

wordavg <- aggregate(rating ~ word,icon2,mean)

### Rename:

names(wordavg) <- c("Word","Written")
names(icon1) <- c("POS","Word","Written","Spoken","AlienAccuracy")

### Merge:

icon <- rbind(select(icon1,Word,Written),wordavg)

### Create a list identifier:

icon$ListIdentifier <- c(rep("list1",nrow(icon1)),rep("list2",nrow(wordavg)))

### Load in additional data:

dantzig <- read.csv("/Users/teeniematlock/Desktop/big_data/van_dantzig_property/13428_2010_38_MOESM1_ESM.csv")
lynott <- read.csv("/Users/teeniematlock/Desktop/big_data/lynott_modality_norms/lynott_norms.csv")
lynott_nouns <- read.csv("/Users/teeniematlock/Desktop/big_data/lynott_noun_norms/lynott_noun_norms.csv")
warriner <- read.csv("/Users/teeniematlock/Desktop/big_data/warriner_affective_ratings/Ratings_Warriner_et_al.csv")
AOA <- read.csv("/Users/teeniematlock/Desktop/big_data/kuperman_AOA/AOA_kuperman.csv")
SUBTLEX <- read.csv("/Users/teeniematlock/Desktop/big_data/SUBTLEX_US/SUBTLEX_US_with_POS.csv")
conc <- read.csv("/Users/teeniematlock/Desktop/big_data/brysbaert_concreteness_ratings/brysbaert_concreteness.csv")

### Rename columns so they can be easily merged:

dantzig <- rename(dantzig,Word=Property)
lynott <- rename(lynott,Word=Property)
lynott_nouns <- rename(lynott_nouns,Word=Noun,DominantModalityNoun=DominantModality)

### Merge the dantzig columns for the two concepts:

dantzig <- mutate(dantzig,DantzigAuditory=(MeanAud1+MeanAud2)/2,
	DantzigGustatory=(MeanGus1+MeanGus2)/2,
	DantzigOlfactory=(MeanOlf1+MeanOlf2)/2,
	DantzigVisual=(MeanVis1+MeanVis2)/2,
	DantzigHaptic=(MeanHap1+MeanHap2)/2)

### Get the new main sensory modality:

dantzig$DantzigDominantModality <- apply(dantzig[,grep("Dantzig",names(dantzig))],1,which.max)
dantzig$DantzigDominantModality <- gsub("Dantzig","",
	grep("Dantzig",names(dantzig),value=T)[dantzig$DantzigDominantModality])

### Rename the lynott noun norm columns so that there's no clash of column names:

names(lynott) <- paste0("LynottNoun",names(lynott))

### Create Warriner absolut valence variable and rename that ugly valence column name:

warriner$AbsValence <- abs(warriner$V.Mean.Sum-mean(warriner$V.Mean.Sum))

### Rename columns for identifiability:

warriner <- rename(warriner,Valence=V.Mean.Sum)
SUBTLEX <- rename(SUBTLEX,POS=Dom_PoS_SUBTLEX,WordFreq=Lg10WF)

### Merge all information:

icon <- cbind(icon,
	dantzig[match(icon$Word,dantzig$Word),grep("Dantzig",names(dantzig))],
	lynott[match(icon$Word,lynott$Word),grep("Mean|Dominant",names(lynott))],
	lynott_nouns[match(icon$Word,lynott_nouns$Word),grep("Mean|Dominant",names(lynott))],
	warriner[match(icon$Word,warriner$Word),c("Valence","AbsValence")],
	data.frame(AOA=AOA[match(icon$Word,AOA$Word),]$Rating.Mean),
	data.frame(Conc=conc[match(icon$Word,conc$Word),]$Conc.M),
	SUBTLEX[match(icon$Word,SUBTLEX$Word),c("POS","WordFreq")],
	data.frame(OriginalPOS=icon1[match(icon$Word,icon1$Word),]$POS))

### Create a content POS column:

icon$ContentPOS <- icon$POS
icon[!(icon$ContentPOS %in% c("Adjective","Adverb","Noun","Verb")),]$ContentPOS = NA
icon$ContentPOS <- factor(icon$ContentPOS)

### Write to table:

write.table(icon,"iconicity_ratings_both.csv",sep=",",row.names=F)


