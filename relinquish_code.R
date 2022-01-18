
####PACKAGES ####

library(ggplot2)
library(shapefiles)
library(readxl)
library(dplyr)
library(nnet)
library(xlsReadWrite)
library(tidyverse)
library(dplyr)
library(fastDummies)

####PRIMARY SURRENDER REASON####
 
owner_all_relinquish<-read.csv("/Users/baileyhe/Desktop/R Projects/Dog Surrender/fresh_data.csv", stringsAsFactors = FALSE)

View(owner_all_relinquish)

unique(owner_all_relinquish$Surrender.Reason)

owner_all_relinquish$surrender.category[grepl('Abandoned', owner_all_relinquish$Surrender.Reason)]<- "Abandoned"

owner_all_relinquish$surrender.category[owner_all_relinquish$Surrender.Reason == "Pregnant - Animal Is Pregnant"|
                                          owner_all_relinquish$Surrender.Reason == "Upper Respiratory Infection (uri)"|
                                          owner_all_relinquish$Surrender.Reason == "Urinary Tract Infection (UTI)"|
                                          owner_all_relinquish$Surrender.Reason == "Animal Had Debilitating Injury"|
                                          owner_all_relinquish$Surrender.Reason == "Dog got too Big"]<- "Animal Health"

owner_all_relinquish$surrender.category[grepl('Behaviour', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Aggression', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Aggressive', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Bit', owner_all_relinquish$Surrender.Reason)|
                                          owner_all_relinquish$Surrender.Reason == "Attacking Livestock"|
                                          owner_all_relinquish$Surrender.Reason == "Food Protective"|
                                          owner_all_relinquish$Surrender.Reason == "Barking"|
                                          owner_all_relinquish$Surrender.Reason == "Chewing"|
                                          grepl('Digging', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Doesnt Like', owner_all_relinquish$Surrender.Reason)|
                                          owner_all_relinquish$Surrender.Reason == "Home - Pet Is Showing Signs Of Stress In The Home"|
                                          owner_all_relinquish$Surrender.Reason == "Jumping The Fence"|
                                          owner_all_relinquish$Surrender.Reason == "Marking / Spraying"|
                                          owner_all_relinquish$Surrender.Reason == "Not Housebroken"|
                                          owner_all_relinquish$Surrender.Reason == "Not Good With Cats"|
                                          owner_all_relinquish$Surrender.Reason == "Problems With Other Pets"|
                                          owner_all_relinquish$Surrender.Reason == "Reactive House Soiling (situational/stress)"|
                                          owner_all_relinquish$Surrender.Reason == "Separation Anxiety"|
                                          owner_all_relinquish$Surrender.Reason == "Snapping At Children"]<-"Behaviour"

owner_all_relinquish$surrender.category[grepl('Cant Afford', owner_all_relinquish$Surrender.Reason)|
                                          owner_all_relinquish$Surrender.Reason == "Vet Costs"|
                                          owner_all_relinquish$Surrender.Reason == "Wont Pay Impound Fees"]<-"Cant Afford"

owner_all_relinquish$surrender.category[grepl('Feral', owner_all_relinquish$Surrender.Reason)|
                                          owner_all_relinquish$Surrender.Reason == "Feral/Free Roaming - Living on Person's Property"]<-"CommunityDog"

owner_all_relinquish$surrender.category[grepl('Allergies', owner_all_relinquish$Surrender.Reason)|
                                          owner_all_relinquish$Surrender.Reason == "Death Of Owner"|
                                          owner_all_relinquish$Surrender.Reason == "Life Change-Human Illness/Injury/CareHome/Hospital"|
                                          owner_all_relinquish$Surrender.Reason == "Owner Had Debilitating Injury"|
                                          owner_all_relinquish$Surrender.Reason == "Owner Sick"|
                                          owner_all_relinquish$Surrender.Reason == "Treatment Centre"]<-"Guardian Health"

owner_all_relinquish$surrender.category[grepl('Moving/Evicted', owner_all_relinquish$Surrender.Reason)|
                                          owner_all_relinquish$Surrender.Reason == "Eviction"|
                                          owner_all_relinquish$Surrender.Reason == "Moving House" | 
                                          owner_all_relinquish$Surrender.Reason == "Abandoned By Tenant During Move"| 
                                          owner_all_relinquish$Surrender.Reason == "Landlord Does Not Allow Pets"|
                                          owner_all_relinquish$Surrender.Reason == "Unable To Find Home"|
                                          owner_all_relinquish$Surrender.Reason == "Yard Too Small"|
                                          owner_all_relinquish$Surrender.Reason == "Complaints From Neighbour"|
                                          owner_all_relinquish$Surrender.Reason == "Military Transfer"|
                                          owner_all_relinquish$Surrender.Reason == "Holidays/Travel"|
                                          owner_all_relinquish$Surrender.Reason == "Holidays/travel"]<-"Housing Issues"

owner_all_relinquish$surrender.category[owner_all_relinquish$Surrender.Reason == "Gift"|
                                          owner_all_relinquish$Surrender.Reason == "No Longer Wanted"]<-"No Longer Wanted"


owner_all_relinquish$surrender.category[grepl('Divorce', owner_all_relinquish$Surrender.Reason)|
                                          grepl('New Baby', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Relation Split', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Jail', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Owner Pregnant', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Abusive Relationship', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Constable Visit', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Children Not Ready', owner_all_relinquish$Surrender.Reason)|
                                          grepl('No Time', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Too Much Responsibility', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Children Not Ready', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Not Due To Cost', owner_all_relinquish$Surrender.Reason)|
                                          grepl('Constable Visit', owner_all_relinquish$Surrender.Reason)]<-"Personal Issues"

owner_all_relinquish$surrender.category[grepl('Too Many', owner_all_relinquish$Surrender.Reason)|
                                          owner_all_relinquish$Surrender.Reason == "Unwanted Litter"]<-"Too Many"

owner_all_relinquish$surrender.category[owner_all_relinquish$Surrender.Reason == "Other"]<-"Other"

owner_all_relinquish$surrender.category[owner_all_relinquish$Surrender.Reason == "Dog got too Big"]<-"Animal Characteristics"

####SECONDARY SURRENDER REASON####

unique(relinquish$surrender.category)

housing_secondary <- relinquish %>% 
  filter (surrender.category =="Housing Issues")

personal_secondary <- relinquish %>% 
  filter (surrender.category =="Personal Issues")

cost_secondary <- relinquish %>% 
  filter (surrender.category =="Cant Afford")

guardianhealth_secondary <- relinquish %>% 
  filter (surrender.category =="Guardian Health")

behaviour_secondary <- relinquish %>% 
  filter (surrender.category =="Behaviour")

#housing

unique(housing_secondary$Surrender.Reason)

owner_all_relinquish$surrender.secondary[grepl('Moving House', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Military Transfer"]<-"Moving"

owner_all_relinquish$surrender.secondary[grepl('Landlord Does Not Allow Pets', owner_all_relinquish$Surrender.Reason)|
                                          owner_all_relinquish$Surrender.Reason == "Moving/Evicted - Cant Take Pet Where Owner Going"|
                                          owner_all_relinquish$Surrender.Reason == "Moving/Evicted - Cant Afford Pet Friendly Housing"|
                                          owner_all_relinquish$Surrender.Reason == "Moving/Evicted - Cant Find Pet Friendly Housing"]<-"Pet Housing"

owner_all_relinquish$surrender.secondary[grepl('Eviction', owner_all_relinquish$Surrender.Reason)]<- "Eviction"

owner_all_relinquish$surrender.secondary[grepl('Complaints From Neighbour', owner_all_relinquish$Surrender.Reason)]<- "Complaint"

#personal 

unique(personal_secondary$Surrender.Reason)

owner_all_relinquish$surrender.secondary[grepl('No Time', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Too Much Responsibility"|
                                           owner_all_relinquish$Surrender.Reason == "No Time For Pet/Pet Is Too Much Responsibility"|
                                           owner_all_relinquish$Surrender.Reason == "Cant Focus On Pets Medical Needs (Not Due To Cost)"]<-"Personal Animal Care"

owner_all_relinquish$surrender.secondary[grepl('Relation Split', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "New Baby"|
                                           owner_all_relinquish$Surrender.Reason == "Divorce"|
                                           owner_all_relinquish$Surrender.Reason == "Life Change - New Baby/Divorce/Relation Split/etc."|
                                           owner_all_relinquish$Surrender.Reason == "Home - Children Not Ready/Not Good With Pet"|
                                           owner_all_relinquish$Surrender.Reason == "Owner Pregnant"]<-"Personal Family"

owner_all_relinquish$surrender.secondary[grepl('Home - Abusive Relationship/Violence In Family', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Life Change- Jail/Treatment Centre/Transition Home"|
                                           owner_all_relinquish$Surrender.Reason == "Constable Visit"|
                                           owner_all_relinquish$Surrender.Reason == "Jail"]<-"Personal Legal"

#cant afford

unique(cost_secondary$Surrender.Reason)

owner_all_relinquish$surrender.secondary[grepl('Vet Costs', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Cant Afford Vet Expenses for Procedure/Illness"|
                                           owner_all_relinquish$Surrender.Reason == "Cant Afford Vet Expenses for Chronic Condition"|
                                           owner_all_relinquish$Surrender.Reason == "Cant Afford Spay/Neuter"]<-"Cost Veterinary"

owner_all_relinquish$surrender.secondary[grepl('Wont Pay Impound Fees', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Cant Afford Food/Grooming/General Expenses"]<-"Cost General Care"

#guardian health

unique(guardianhealth_secondary$Surrender.Reason)

owner_all_relinquish$surrender.secondary[grepl('Death Of Owner', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Life Change-Human Illness/Injury/CareHome/Hospital"|
                                           owner_all_relinquish$Surrender.Reason == "Owner Sick"|
                                           owner_all_relinquish$Surrender.Reason == "Treatment Centre"|
                                           owner_all_relinquish$Surrender.Reason == "Owner Had Debilitating Injury"]<-"Owner Illness/Injury"

owner_all_relinquish$surrender.secondary[grepl('Owner Had Allergies', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Allergies - Person Has Allergies To Pet"]<-"Owner Allergies"

#behaviour

unique(behaviour_secondary$Surrender.Reason)

owner_all_relinquish$surrender.secondary[grepl('Aggression', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Aggressive To Other Dogs"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Bit Another Animal"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Growls/Lunges/Claws/Hisses At People"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Killed A Companion Animal"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Growls/Lunges/Claws/Hisses At Animals"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Bit A Person"|
                                           owner_all_relinquish$Surrender.Reason == "Snapping At Children"|
                                           owner_all_relinquish$Surrender.Reason == "Food Protective"|
                                           owner_all_relinquish$Surrender.Reason == "Biting"]<-"Aggression"

owner_all_relinquish$surrender.secondary[grepl('Behaviour - Keeps Escaping', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Too Active"|
                                           owner_all_relinquish$Surrender.Reason == "Jumping The Fence"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Chases Cats/Wildlife/Bicycles/Cars/etc"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Too Vocal (Barking/Meowing/Howls/etc.)"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Destructive (Chews/Digs/Scratches/etc)"|
                                           owner_all_relinquish$Surrender.Reason == "Barking"|
                                           owner_all_relinquish$Surrender.Reason == "Digging Yard"|
                                           owner_all_relinquish$Surrender.Reason == "Chewing"|
                                           owner_all_relinquish$Surrender.Reason == "Digging Out"]<-"Objectionable Activity"

owner_all_relinquish$surrender.secondary[grepl('Problems With Other Pets', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Home - Other Pets Aggressive To Him/Her"|
                                           owner_all_relinquish$Surrender.Reason == "Not Good With Cats"]<-"Animal Conflict"

owner_all_relinquish$surrender.secondary[grepl('Behaviour - Pet Is Not Good With Children', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Doesnt Like Women"|
                                           owner_all_relinquish$Surrender.Reason == "Doesnt Like Men"]<-"Person Conflict"

owner_all_relinquish$surrender.secondary[grepl('Behaviour - Housetraining/Spraying/Marking Issues', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Not Housebroken"|
                                           owner_all_relinquish$Surrender.Reason == "Reactive House Soiling (situational/stress)"|
                                           owner_all_relinquish$Surrender.Reason == "Marking / Spraying"]<-"House Soiling"

owner_all_relinquish$surrender.secondary[grepl('Attacking Livestock', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Killed A Livestock/Farm Animal"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Chases Livestock/Farm Animals"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Killed Wildlife"]<-"Predation"

owner_all_relinquish$surrender.secondary[grepl('Separation Anxiety', owner_all_relinquish$Surrender.Reason)|
                                           owner_all_relinquish$Surrender.Reason == "Home - Pet Is Showing Signs Of Stress In The Home"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Pet Has Anxiety"|
                                           owner_all_relinquish$Surrender.Reason == "Behaviour - Undersocialized"]<-"Anxiety Stress"

####SURRENDER TYPE GUARDIAN ANIMAL RELATED####

unique(owner_all_relinquish$surrender.category)

owner_all_relinquish$surrender.type[grepl('Too Many', owner_all_relinquish$surrender.category)|
                                       grepl('Personal Issues', owner_all_relinquish$surrender.category)|
                                       grepl('No Longer Wanted', owner_all_relinquish$surrender.category)|
                                       grepl('Housing Issues', owner_all_relinquish$surrender.category)|
                                       grepl('Guardian Health', owner_all_relinquish$surrender.category)|
                                       grepl('Cant Afford', owner_all_relinquish$surrender.category)]<-"Guardian Related"

owner_all_relinquish$surrender.type[grepl('Behaviour', owner_all_relinquish$surrender.category)|
                                      grepl('Animal Health', owner_all_relinquish$surrender.category)|
                                      grepl('Community Dog', owner_all_relinquish$surrender.category)|
                                      grepl('Animal Characteristics', owner_all_relinquish$surrender.category)]<-"Animal Related"

unique(owner_all_relinquish$surrender.type)


####BREED TYPE####

owner_all_relinquish$breed_group <- ifelse( grepl("Mix",owner_all_relinquish$Breed), "Mix",
                                            ifelse(grepl("/",owner_all_relinquish$Breed), "Mix",
                                                   ifelse(is.na(owner_all_relinquish$Breed), NA(),
                                                          "Suspected Purebred")))


####POPULATION####

owner_all_relinquish$population[grepl('Vancouver Branch', owner_all_relinquish$Incoming.Region)|
                                      grepl('Victoria Branch', owner_all_relinquish$Incoming.Region)|
                                      grepl('Kelowna Branch', owner_all_relinquish$Incoming.Region)|
                                      grepl('Burnaby', owner_all_relinquish$Incoming.Region)|
                                      grepl('Surrey Education & Adoption Centre', owner_all_relinquish$Incoming.Region)|
                                      grepl('Richmond Education & Adoption Centre', owner_all_relinquish$Incoming.Region)|
                                      grepl('Abbotsford Branch', owner_all_relinquish$Incoming.Region)]<-"Large Urban"


owner_all_relinquish$population[grepl('Nanaimo & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Kamloops & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Chilliwack Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Vernon & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Maple Ridge Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('North Cariboo District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('South Okanagan/Similkameen Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Boundary', owner_all_relinquish$Incoming.Region)|
                                          grepl('Campbell River Branch', owner_all_relinquish$Incoming.Region)]<-"Medium"

owner_all_relinquish$population[grepl('Parksville-Qualicum Beach & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Alberni-Clayoquot Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Williams Lake & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Cowichan & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Powell River & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Prince Rupert Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Quesnel & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Nelson Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Williams Lake & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Haida Gwaii Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Comox', owner_all_relinquish$Incoming.Region)|
                                          grepl('South Peace Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('West Kootenay & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('East Kootenay Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Shuswap Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Sunshine Coast Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('North Peace Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Sea To Sky Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('100 Mile House & District Branch', owner_all_relinquish$Incoming.Region)|
                                          grepl('Salt Spring Island Branch', owner_all_relinquish$Incoming.Region)]<-"Small"

unique(owner_all_relinquish$population)

####AGE GROUPS####

unique(owner_all_relinquish$Age.Group)

owner_all_relinquish$age.category[grepl('Puppy', owner_all_relinquish$Age.Group)|
                                          grepl('Kitten', owner_all_relinquish$Age.Group)|
                                          grepl('Baby', owner_all_relinquish$Age.Group)]<-"Puppy"

owner_all_relinquish$age.category[grepl('Young Adult', owner_all_relinquish$Age.Group)|
                                    grepl('Juvenile', owner_all_relinquish$Age.Group)]<-"Young Adult"

owner_all_relinquish$age.category[owner_all_relinquish$Age.Group == "Adult"]<-"Adult"

owner_all_relinquish$age.category[grepl('Geriatric', owner_all_relinquish$Age.Group)|
                                    grepl('Senior', owner_all_relinquish$Age.Group)]<-"Senior"

####ASILOMAR####

unique(owner_all_relinquish$First.Eval.Category.Assigned.From.Intake)

owner_all_relinquish$asilomar[owner_all_relinquish$First.Eval.Category.Assigned.From.Intake == "Treatable-Manageable"]<-"Treatable-Manageable"
owner_all_relinquish$asilomar[owner_all_relinquish$First.Eval.Category.Assigned.From.Intake == "Healthy"]<-"Healthy"
owner_all_relinquish$asilomar[owner_all_relinquish$First.Eval.Category.Assigned.From.Intake == "Treatable-Rehabilitatable"]<-"Treatable-Rehabilitatable"
owner_all_relinquish$asilomar[owner_all_relinquish$First.Eval.Category.Assigned.From.Intake == "Unhealthy-Untreatable"]<-"Unhealthy-Untreatable"


####SPAY NEUT####

unique(owner_all_relinquish$Incoming.Spayed...Neutered.Status)

owner_all_relinquish$spayneut[owner_all_relinquish$Incoming.Spayed...Neutered.Status == "Yes"]<-"Yes"
owner_all_relinquish$spayneut[owner_all_relinquish$Incoming.Spayed...Neutered.Status == "No"]<-"No"

####FURTHER BEHAVIOUR CATEGORIES####

behaviour_only<-owner_all_relinquish %>% 
  filter(surrender.category =="Behaviour")

unique(behaviour_only$Surrender.Reason)

####SIZE####

relinquish <- owner_all_relinquish %>% mutate(Size = 
                                                case_when(Weight..Closest.to.Incoming.Date.<= 20 ~ "Smaller", 
                                                          Weight..Closest.to.Incoming.Date. >= 21 ~ "Larger"))

colnames(relinquish)

####PREPARING DATA FOR ANALYSIS####

relinquish <- export_dataset

relinquish_analysis <- dummy_cols(relinquish, select_columns = 'surrender.category')

####BINARY LOGISITIC REGRESSIONS####

colnames(relinquish)

toomany.logit <- glm(relinquish_analysis$`surrender.category_Too Many` ~
                       relinquish_analysis$year +
                       relinquish_analysis$age.category +
                       relinquish_analysis$Size +
                       relinquish_analysis$breed_group +
                       relinquish_analysis$Gender+
                       relinquish_analysis$asilomar + 
                       relinquish_analysis$population,
                     family = "binomial")

summary(toomany.logit)
exp(cbind(OR = coef(toomany.logit), confint(toomany.logit)))

housing.logit <- glm(relinquish_analysis$`surrender.category_Housing Issues` ~
                       relinquish_analysis$year +
                       relinquish_analysis$age.category +
                       relinquish_analysis$Size +
                       relinquish_analysis$breed_group +
                       relinquish_analysis$Gender+
                       relinquish_analysis$asilomar + 
                       relinquish_analysis$population,
                     family = "binomial")

summary(housing.logit)
exp(cbind(OR = coef(housing.logit), confint(housing.logit)))

personal.logit <- glm(relinquish_analysis$`surrender.category_Personal Issues` ~
                        relinquish_analysis$year +
                        relinquish_analysis$age.category +
                        relinquish_analysis$Size +
                        relinquish_analysis$breed_group +
                        relinquish_analysis$Gender+
                        relinquish_analysis$asilomar + 
                        relinquish_analysis$population,
                      family = "binomial")

summary(personal.logit)

exp(cbind(OR = coef(personal.logit), confint(personal.logit)))

cost.logit <- glm(relinquish_analysis$`surrender.category_Cant Afford` ~
                    relinquish_analysis$year +
                    relinquish_analysis$age.category +
                    relinquish_analysis$Size +
                    relinquish_analysis$breed_group +
                    relinquish_analysis$Gender+
                    relinquish_analysis$asilomar + 
                    relinquish_analysis$population,
                  family = "binomial")
summary(cost.logit)
exp(cbind(OR = coef(cost.logit), confint(cost.logit)))

behaviour.logit <- glm(relinquish_analysis$`surrender.category_Behaviour` ~
                         relinquish_analysis$year +
                         relinquish_analysis$age.category +
                         relinquish_analysis$Size +
                         relinquish_analysis$breed_group +
                         relinquish_analysis$Gender+
                         relinquish_analysis$asilomar + 
                         relinquish_analysis$population,
                       family = "binomial")
summary(behaviour.logit)
exp(cbind(OR = coef(behaviour.logit), confint(behaviour.logit)))

gh.logit <- glm(relinquish_analysis$`surrender.category_Guardian Health` ~
                        relinquish_analysis$year +
                        relinquish_analysis$age.category +
                        relinquish_analysis$Size +
                        relinquish_analysis$breed_group +
                        relinquish_analysis$Gender+
                        relinquish_analysis$asilomar + 
                        relinquish_analysis$population,
                      family = "binomial")

summary(gh.logit)

exp(cbind(OR = coef(gh.logit), confint(gh.logit)))

####MAKING SUMMARY TABLES####

count(relinquish)

unique(relinquish$age.category)

puppy <- relinquish %>% 
  filter (relinquish$age.category =="Puppy")

adult <- filter(relinquish,age.category=='Adult'| age.category=='Senior'| age.category=='Young Adult')
  
table(relinquish$surrender.type)
table(relinquish$surrender.category)
table(relinquish$year)
table(relinquish$age.category)
table(relinquish$spayneut)
table(relinquish$asilomar)
table(relinquish$Size)
table(relinquish$breed_group)
table(relinquish$population)
table(relinquish$population, relinquish$spayneut)

count(puppy)
table(puppy$surrender.type)
table(puppy$surrender.category)
table(puppy$surrender.secondary)
table(puppy$year)

count(adult)
table(adult$surrender.type)
table(adult$surrender.category)
table(adult$surrender.secondary)
table(adult$year)

####EXPORT TO CSV####

write.csv(relinquish,"/Users/baileyhe/Desktop/R Projects/Dog Surrender/export_dataset.csv", row.names = FALSE)

