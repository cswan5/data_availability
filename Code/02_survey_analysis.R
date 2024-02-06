install.packages("ggflags", repos = c(
  "https://jimjam-slam.r-universe.dev",
  "https://cloud.r-project.org"))

library(tidyverse)
library(patchwork)
library(ggflags)
library(countrycode)

## Read in the survey data
dat <- read.csv("./Data/survey_clean.csv") 

## Calculate the completion rate for the survey
comp <- length(unique(dat$UserID))

comp_perc <- comp/472

##############################
## Demographics information ##
##############################

## Merge the different jobs into overarching categories
demo <- dat %>% 
  select(UserID,Q2,Q3) %>% 
  distinct(UserID,Q2,Q3) %>% 
  mutate(
         Q2 = case_when(Q2 == "membro do MinistÃ©rio PÃºblico Federal" ~ "Government employee",
                        str_detect(Q2,"NGO Consultant|Philanthropic Program Officer") ~ "NGO employee",
                        str_detect(Q2,"Consultor ambiental|independent scholar and consultant|Private sector employee|pesquisador autÃ´nomo") ~ "Private sector",
                        TRUE ~ Q2),
         work_cat = case_when(Q2=="Postdoctoral researcher"|
                                Q2 == "Graduate student"|
                                Q2 == "Academic faculty" ~ "Academic",
                              Q2 == "membro do MinistÃ©rio PÃºblico Federal"|
                                Q2 == "Government employee" ~ "Government",
                              Q2 == "NGO Consultant"|
                                Q2 == "NGO employee"|
                                Q2 == "Philanthropic Program Officer" ~ "NGO",
                              Q2 == "Consultor ambiental"|
                                Q2 == "independent scholar and consultant"|
                                Q2 == "Private sector" ~ "Private sector"),
         location = case_when(str_detect(Q3,"(?i)Bra.*") ~ "Brazil",
                        str_detect(Q3,"US.*|Unit.*|EUA") ~ "United States",
                        Q3 == "CHINA" ~ "China",
                        Q3 == "Holanda" ~ "The Netherlands",
                        TRUE ~ Q3),
         iso2 = countrycode(location,"country.name","iso2c"))

demo.summ <- demo %>% 
  group_by(Q2,iso2) %>% 
  count() %>% 
  mutate(country=case_when(iso2=="AT"~ "Austria",
                           iso2=="BR"~"Brazil",
                           iso2=="CN"~"China",
                           iso2=="NL"~"The Netherlands",
                           iso2=="PE"~"Peru",
                           iso2=="US"~"United States"),
         flag_pos=n,
         lab_pos=n+0.25)
demo.summ$Q2 <- fct_relevel(demo.summ$Q2,c("Academic faculty","Postdoctoral researcher",
                                           "Graduate student","Government employee",
                                           "NGO employee","Private sector"))


## Summarize location data (first format data)
loc.summ<- demo %>% 
  mutate(Q3 = case_when(str_detect(Q3,"(?i)Bra.*") ~ "Brazil",
                        str_detect(Q3,"US.*|Unit.*|EUA") ~ "United States",
                        Q3 == "CHINA" ~ "China",
                        TRUE ~ Q3)) %>% 
  count(Q3) %>% 
  mutate(perc = n/sum(n)*100,
         country = case_when(Q3 == "Austria" ~ "at",
                             Q3 == "Brazil" ~ "br",
                             Q3 == "China" ~ "cn",
                             Q3 == "Holanda" ~ "nl",
                             Q3 == "Peru" ~ "pe",
                             Q3 == "United States" ~ "us"),
         Q3 = str_replace(Q3,"Holanda","Netherlands"),
         Q3 = fct_reorder(Q3,n,.desc=TRUE))

fig2a <- ggplot(loc.summ,aes(x=n,y=Q3))+
  geom_bar(stat="identity",width=0.7)+
  geom_flag(aes(x=0,y=Q3,country=country),size=6)+
  labs(x="Number of responses",y="")+
  theme(legend.position=c(0.93,0.75),legend.title=element_blank(),
        panel.background = element_blank(), panel.border=element_rect(fill=NA))+
  ggtitle("(A) Location")


## Summarize job type data
job.summ <- demo %>% 
  count(work_cat) %>% 
  mutate(perc = n/sum(n)*100,
         work_cat = fct_reorder(work_cat,n,.desc=TRUE))

fig2b <- ggplot(job.summ,aes(x=n,y=work_cat))+
  geom_bar(stat="identity",width=0.7)+
  labs(x="Number of responses",y="")+
  theme(legend.position=c(0.93,0.75),legend.title=element_blank(),
        panel.background = element_blank(), panel.border=element_rect(fill=NA))+
  ggtitle("(B) Career type")

## Summarize data about the types of research done
disc <- dat %>% 
  select(UserID,Q4,Q9) %>% 
  distinct(UserID,Q4,Q9) %>% 
  mutate(Q9=str_replace(Q9,"(?<!\\w)Natural Resources \\(Fisheries, timber, non-timber forest products, hunting, etc\\)(?!\\w)",
         "Natural Resources"),
         Q9 = str_replace(Q9,"Hydrology and geomorphology","Hydrology/geomorphology"),
         Q9 = str_replace(Q9,"Indigenous  and/or traditional peoples","Indigenous peoples"),
         Q9 = str_replace(Q9, "Land use/ land cover change","LULCC")) %>% 
  filter(!is.na(Q9)) %>% 
  separate_rows(Q9, sep=",") %>%
  count(Q9) %>% 
  mutate(perc = n/sum(n)*100,
         Q9 = fct_reorder(Q9, n, .desc = TRUE))# %>% 
  
fig2c <- ggplot(disc,aes(x=n,y=Q9))+
  geom_bar(stat="identity")+
  labs(x="Number of projects",y="")+
  theme(legend.position="none",panel.background = element_blank(), panel.border=element_rect(fill=NA))+
  ggtitle("(C) Project themes")

## Summarize by infrastructure types
infra <- dat %>% 
  select(UserID,Q4) %>% 
  count(Q4) %>% 
  mutate(perc = n/sum(n)*100,
         Q4=fct_reorder(Q4,n,.desc=TRUE))

fig2d <- ggplot(infra,aes(x=n,y=Q4))+
  geom_bar(stat="identity")+
  labs(x="Number of searches",y="")+
  theme(panel.background=element_blank(),panel.border = element_rect(fill=NA))+
  ggtitle("(D) Infrastructure")

(fig2a|fig2b)/(fig2c|fig2d)
ggsave("./Figures/Fig2.png",height=8,width=14,dpi=600)

## Summarize by project phase
phase <- dat %>% 
  select(UserID,Q4,Q7) %>% 
  distinct(UserID,Q4,Q7) %>% 
  mutate(Q7=str_replace(Q7,"(?<!\\w)\\(including licensing, project development, approval, financing, etc\\)(?!\\w)",""),
         Q7=str_replace(Q7,"(?<!\\w)\\(including timelines, engineering info, construction company, etc\\)(?!\\w)",""),
         Q7=str_replace(Q7,"(?<!\\w)\\(including monitoring, mitigation efforts, management, etc.\\)(?!\\w)",""),
         Q7=str_replace_all(Q7," ","")) %>% 
  separate_rows(Q7,sep=",") %>% 
  filter(!is.na(Q7)) %>% 
  count(Q7) %>% 
  mutate(perc = n/sum(n)*100)

## Number of projects searched for
nums <- dat %>% 
  select(UserID,Q4,Q8) %>% 
  distinct(UserID,Q4,Q8) %>% 
  mutate(Q8=str_replace(Q8,"(?<!\\w)\\(2-5\\)(?!\\w)",""),
         Q8=str_replace_all(Q8," ,",","),
         Q8=str_replace_all(Q8,"projects $","projects")) %>% 
  separate_rows(Q8,sep=",") %>%
  filter(!is.na(Q8)) %>% 
  count(Q8) %>% 
  mutate(perc=n/sum(n)*100)

## Number of different attributes searched for
## Some of the information in "other" was repeated from what was listed in the
## survey. In those cases, we ensured that these responses were counted as 
## one of the answers that could have been selected. If the attributes filled in
## under "other" were not part of a pre-selected group, we included them as "other"
tech.info <- c("Dados operacionais da barragens, como volume, Ã¡rea inundada entre outros",
               "operations plan peaking, storage, etc",
               "Area do reservatorio e capacidade instalada")
lic.info <- c("dates and process of approval","same as above")

atts <- dat %>% 
  select(UserID,Q4,Q5,Q6) %>% 
  mutate(Q5=str_replace(Q5,"(?<!\\w)\\(e\\.g\\. paved vs dirt road, reservoir vs run\\-of\\-the river\\)(?!\\w)",""),
         Q5=str_replace(Q5,"(?<!\\w)\\(e\\.g\\. current, operational\\)(?!\\w)",""),
         Q5=str_replace(Q5,"(?<!\\w)\\(e.g. capacity, voltage, bandwidth, etc\\)","")) %>% 
  separate_rows(Q5,sep=",") %>% 
  mutate(Q5=case_when(Q5=="Other" & Q6%in%tech.info ~ "Technical information ",
                      Q5=="Other" & Q6%in%lic.info ~ "Licensing dates",
                      Q5=="Other" & Q6=="EIA/RIMA" ~ "Environmental Impact Assessment (EIA) documents",
                      TRUE ~ Q5)) %>% 
  distinct(UserID,Q4,Q5,.keep_all = TRUE) %>% 
  filter(!(Q5 == "Other" & Q6 == "Shapefile"))

## Get the proportion of attributes that were searched for for each type of project
atts.prop <- atts %>% 
  count(Q4,Q5,sort=TRUE) %>%
  group_by(Q5) %>% 
  mutate(n2=sum(n)) %>% 
  ungroup() %>% 
  group_by(Q4) %>% 
  mutate(totals = sum(n),
         prop = n/totals) %>% 
  mutate(infra = case_when(Q4 %in% c("Large hydroelectric dams (>30MW)",
                                     "Small dams (<30 MW)","Solar energy plants",
                                     "Wind energy plants","Thermal energy plants") ~
                                       "Energy generation",
                           Q4 %in% c("Pipelines","Transmission/distribution lines") ~
                                       "Energy distribution",
                           Q4 %in% c("Roads or highways","Waterways (hidrovias)",
                                     "Railroads","Ports") ~ "Transportation",
                           Q4 %in% c("Waste water or sewage") ~ "Sewage",
                           TRUE ~ Q4),
         Q5 = case_when(Q5 == "Precise location" ~ "Point location",
                        Q5 == "Environmental Impact Assessment (EIA) documents" ~ 
                          "EIA documents",
                        Q5 == "Geographic coordinates for the full extent of the project (e.g. exact location of the entire road)" ~
                          "Spatial extent",
                        Q5 == "Owner or operator or construction company" ~
                          "Owner",
                        Q5 == "Budget information" ~ "Budget",
                        TRUE ~ Q5)) %>% 
  ungroup() %>% 
  mutate(Q5 = fct_reorder(Q5,-n2)) %>% 
  filter(!Q4 %in% c("Schools","Hospitals","Airports","Telecommunications"))

## Generate plot for attribute types
fig3<-ggplot(atts.prop,aes(x=Q5,y=prop))+
  geom_bar(stat="identity",color="grey35",width=0.85)+
  facet_wrap(~infra)+
  labs(x="Attribute",y="Proportion of responses")+
  theme(panel.background = element_blank(), panel.border=element_rect(fill=NA),
        axis.text.x = element_text(angle=90))

fig3

ggsave("./Figures/Fig3.png",height=5,width=7)

## Determine where people searched for and found data
search <- dat %>% select(UserID,Q4,Q11,Q13) %>% 
  filter(!is.na(Q11)) %>% 
  mutate(Q11=str_replace(Q11," (?<!\\w)\\(federal, state, or municipal\\)(?!\\w)",""),
         Q11=str_replace(Q11," (?<!\\w)\\(e\\.g\\. Imazon, The Nature Conservancy, etc\\.\\)(?!\\w)",""),
         Q11=str_replace(Q11," (?<!\\w)\\(dissertations, non\\-government data archives, colleagues, etc\\.\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(federal, state, or municipal\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(e\\.g\\. Imazon, The Nature Conservancy, etc\\.\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(dissertations, non\\-government data archives, colleagues, etc\\.\\)(?!\\w)","")) %>% 
  separate_longer_delim(Q11,delim=",") %>% 
  mutate(SF=ifelse(str_detect(Q11,"Brazil")&str_detect(Q13,"Brazil"),"Original source",
              ifelse(str_detect(Q11,"Aca")&str_detect(Q13,"Aca"),"Original source",
              ifelse(str_detect(Q11,"Non")&str_detect(Q13,"Non"),"Original source",
              ifelse(str_detect(Q11,"Other")&str_detect(Q13,"Other"),"Original source",
              ifelse(str_detect(Q13,"never"),"Never found",
              ifelse(str_detect(Q11,"Brazil")&!str_detect(Q13,"Brazil|never"),"Other source",
              ifelse(str_detect(Q11,"Aca")&!str_detect(Q13,"Aca|never"),"Other source",
              ifelse(str_detect(Q11,"Non")&!str_detect(Q13,"Non|never"),"Other source",
              ifelse(str_detect(Q11,"Other")&!str_detect(Q13,"Other|never"),"Other source","NA")))))))))) %>% 
  separate_longer_delim(SF,delim=",") %>% 
  mutate(Q11=case_when(str_detect(Q11,"Brazil")~"Government",
                       str_detect(Q11,"Aca")~"Academic",
                       str_detect(Q11,"Non")~"NGO",
                       TRUE ~ Q11))

sf <- search %>% group_by(Q11) %>% 
  filter(!is.na(SF)) %>% 
  count(SF) %>% 
  mutate(prop=n/sum(n),
         Q11=as.factor(Q11),
         Q11=fct_relevel(Q11,c("Government","Academic","NGO","Other")),
         SF=as.factor(SF),
         SF=fct_relevel(SF,c("Original source","Other source","Never found")))



ggplot(sf,aes(x=Q11,y=n,fill=SF))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c("#313695","#fdae61","#a50026"), labels= c("Found from original source",
                                                                       "Found, but from a different source",
                                                                       "Never found"))+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  labs(x="Data source",y="Number of datasets")+
  theme(panel.background = element_blank(), panel.border=element_rect(fill=NA))

ggsave("./Figures/Fig5.png",height=4, width=6,dpi=600)

sf.props <- sf %>% 
  group_by(Q11) %>% 
  mutate(tots = sum(n)) %>% 
  ungroup() %>% 
  mutate(all.tot=sum(n),
         prop_fnd = n/tots,
         prop = tots/all.tot) #%>% 
  group_by(Q11) %>% 
  mutate(prop=n/all.tot)

## Measuring data quality
dat.qual <- dat %>% 
  select(UserID,Q4,Q13,Q25,Q26) %>% 
  mutate(Q13=str_replace(Q13," (?<!\\w)\\(federal, state, or municipal\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(e\\.g\\. Imazon, The Nature Conservancy, etc\\.\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(dissertations, non\\-government data archives, colleagues, etc\\.\\)(?!\\w)",""),
         Gov=case_when(str_detect(Q13,"Brazilian")~1,
                       TRUE ~ 0),
         Academic=case_when(str_detect(Q13,"Academic")~1,
                            TRUE ~ 0),
         NGO = case_when(str_detect(Q13,"Non\\-government")~1,
                         TRUE ~ 0),
         Other = case_when(str_detect(Q13,"Other")~1,
                           TRUE ~ 0),
         sum = Gov + Academic + NGO + Other,
         source = case_when(Gov == 1 & sum == 1 ~ "Government",
                            Gov == 0 & sum >= 1 ~ "Non-government")) %>% 
  separate_rows(Q13,sep=",") %>%
  mutate(Q25 = case_when(str_detect(Q25,"moderately")~"Moderate",
                         str_detect(Q25,"Yes")~"High",
                         str_detect(Q25,"did not") ~ "NA"),
         Q26 = case_when(str_detect(Q26,"moderately") ~ "Moderate",
                         str_detect(Q26, "I don't know") ~ "Unsure",
                         str_detect(Q26, "innacurate") ~ "Low",
                         str_detect(Q26, "Yes") ~ "High")) %>%
  pivot_longer(cols=c(Q25,Q26),names_to = "spat",values_to = "accuracy") %>% 
  mutate(spat = case_when(spat == "Q25" ~ "Spatial data",
                          spat == "Q26" ~ "Non-spatial data")) %>% 
  filter(!is.na(source)) %>% 
  group_by(source,spat) %>% 
  count(accuracy) %>% 
  filter(!is.na(accuracy) & accuracy != "NA") %>% 
  mutate(perc = n/sum(n)*100)

dat.qual$accuracy = fct_relevel(dat.qual$accuracy,c("Unsure","Low","Moderate","High"))
dat.qual$source = fct_relevel(dat.qual$source,c("Non-government","Government"))

## Task-independent standards  
task.ind <- dat %>% 
  select(UserID,Q4,Q13,Q22,Q23,Q24,Q27) %>% 
  mutate(Q13=str_replace(Q13," (?<!\\w)\\(federal, state, or municipal\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(e\\.g\\. Imazon, The Nature Conservancy, etc\\.\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(dissertations, non\\-government data archives, colleagues, etc\\.\\)(?!\\w)",""),
         Gov=case_when(str_detect(Q13,"Brazilian")~1,
                       TRUE ~ 0),
         Academic=case_when(str_detect(Q13,"Academic")~1,
                            TRUE ~ 0),
         NGO = case_when(str_detect(Q13,"Non\\-government")~1,
                         TRUE ~ 0),
         Other = case_when(str_detect(Q13,"Other")~1,
                           TRUE ~ 0),
         sum = Gov + Academic + NGO + Other,
         source = case_when(Gov == 1 & sum == 1 ~ "Government",
                            Gov == 0 & sum >= 1 ~ "Non-government")) %>% 
  separate_rows(Q13,sep=",") %>%
  filter(!is.na(source)) %>% 
  mutate(Q22 = case_when(str_detect(Q22, "Yes") ~ "Yes",
                         str_detect(Q22, "not") ~ "No"),
         Q23 = case_when(str_detect(Q23,"included some") ~ "No",
                         str_detect(Q23, "did not require") ~ "NA",
                         str_detect(Q23, "had all the information") ~ "Yes"),
         Q24 = case_when(str_detect(Q24, "Yes") ~ "Yes",
                         str_detect(Q24, "not") ~ "No"),
         Q27 = case_when(str_detect(Q27, "Yes") ~ "Yes",
                         str_detect(Q27, "No") ~ "No")) %>% 
  pivot_longer(cols=c(Q22,Q23,Q24,Q27),names_to="Standard",values_to="YN") %>% 
  mutate(Standard = case_when(Standard == "Q22" ~ "Machine readable",
                              Standard == "Q23" ~ "Metadata quality",
                              Standard == "Q24" ~ "Current",
                              Standard == "Q27" ~ "Complete")) %>% 
  #filter(!is.na(YN)) %>% 
  group_by(source,Standard) %>% 
  count(YN) %>% 
  mutate(perc = n/sum(n)*100) 

task.ind$source=fct_relevel(task.ind$source,c("Non-government","Government"))

## Graphics for task-independent standards and data/metadata quality
qual <- ggplot(dat.qual,aes(x=perc,y=source,fill=accuracy))+
  geom_bar(stat="identity")+
  geom_text(aes(label=paste0(round(perc,0),"%")),color="white",position = position_stack(vjust = 0.5))+
  facet_wrap(~spat,ncol=1)+
  scale_fill_manual(values=c("#fdae61","#74add1","#4575b4","#313695"),
                    labels = c("Unsure","Low","Moderate","High"),name = "Response")+
  theme(legend.position="bottom", panel.border=element_rect(fill=NA),axis.line = element_line(color = "black"),
        panel.background=element_blank())+
  labs(y="",x="Frequency (%)")+
  ggtitle("(A) Data accuracy")

ti <- ggplot(task.ind,aes(x=perc,y=source,fill=YN))+
  geom_bar(stat="identity")+
  geom_text(aes(label=paste0(round(perc,0),"%")),color="white",position=position_stack(vjust=0.5))+
  facet_wrap(~Standard,ncol=1)+
  scale_fill_manual(values=c("#fdae61","#a50026","#313695"),
                    labels = c("N/A","No/Low","Yes/High"),name = "Response")+
  theme(legend.position="bottom", panel.border=element_rect(fill=NA),axis.line = element_line(color = "black"),
        panel.background=element_blank())+
  labs(y="",x="Frequency (%)")+
  ggtitle("(B) Task-independent standards")

qual/ti
ggsave("./Figures/Fig6.png",height=9,width=10,dpi=600)

## Time spent searching for data and making it usable
time.spent <- dat %>% 
  select(UserID,Q4,Q11,Q13,Q21,Q28) %>% 
  mutate(Q13=str_replace(Q13," (?<!\\w)\\(federal, state, or municipal\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(e\\.g\\. Imazon, The Nature Conservancy, etc\\.\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(dissertations, non\\-government data archives, colleagues, etc\\.\\)(?!\\w)",""),
         Gov_format=case_when(str_detect(Q13,"Brazilian")~1,
                       TRUE ~ 0),
         Academic_format=case_when(str_detect(Q13,"Academic")~1,
                            TRUE ~ 0),
         NGO_format = case_when(str_detect(Q13,"Non\\-government")~1,
                         TRUE ~ 0),
         Other_format = case_when(str_detect(Q13,"Other")~1,
                           TRUE ~ 0),
         sum_format = Gov_format + Academic_format + NGO_format + Other_format,
         source = case_when(Gov_format == 1 & sum_format == 1 ~ "Government only",
                            Gov_format == 0 & sum_format >= 1 ~ "Non-government only",
                            sum_format > 0 ~ "Both")) %>% 
  separate_rows(Q13,sep=",") %>% 
  mutate(Q21 = case_when(str_detect(Q13,"never") ~ "Never found",
                         TRUE ~ Q21)) %>% 
  pivot_longer(cols=c(Q21,Q28),names_to = "activity",values_to="time") %>% 
  filter(!is.na(time)) %>% 
  mutate(activity = case_when(activity == "Q21" ~ "Time spent searching for data",
                              activity == "Q28" ~ "Time spent formatting data")) %>% 
  separate_rows(time,sep=",") %>% 
  mutate(time = case_when(time == "<1 hour" ~ "< 1 hr",
                          str_detect(time,"1\\-4|4\\-8") ~ "1-8 hrs",
                          time == "8-24 hours" ~ "8-24 hrs",
                          str_detect(time,"1 day\\-") ~ "24-168 hrs",
                          time == "> 1 week" ~ "> 168 hrs",
                          str_detect(time, "never") ~ "Never usable",
                          str_detect(time, "already") ~ "None",
                          TRUE ~ time),
         time = fct_relevel(time,c("None","< 1 hr","1-8 hrs","8-24 hrs","24-168 hrs",
                                   "> 168 hrs", "Never found/usable")),
         activity = fct_relevel(activity,c("Time spent searching for data",
                                           "Time spent formatting data"))) %>% 
  distinct(UserID,Q4,source,activity,time) %>% 
  group_by(source,activity) %>% 
  count(time) %>% 
    ungroup() %>% 
  group_by(activity, time) %>% 
  mutate(n = case_when(source == "Both" ~ sum(n),
                       TRUE ~ n),
         source = case_when(source == "Both" ~ "All sources",
                            time == "Never found" ~ "All sources",
                            TRUE ~ source)) %>% 
  ungroup() %>% 
  group_by(source,activity) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  filter(!is.na(source))

time.spent$source <- fct_relevel(time.spent$source,c("Government only","Non-government only","All sources"))

ggplot(time.spent,aes(x=time,y=n,fill=source))+
  geom_bar(stat="identity",position=position_dodge(preserve="single"))+
  facet_wrap(~activity,scales="free_x")+
  labs(x="Time spent",y="Number of datasets")+
  scale_fill_manual(values=c("#a50026","#fdae61","#313695"),
                    labels = c("Government only","Non-government only","All sources"),name = "Source")+
  theme(legend.position="bottom", panel.border=element_rect(fill=NA),axis.line = element_line(color = "black"),
        panel.background=element_blank(),axis.text.x = element_text(angle = 60,hjust=1))

ggsave("./Figures/Fig7.png", height=4,width=7,dpi=600)

## Total numbers for time spent searching/formatting (including never found)
time.tots <- dat %>% 
  select(UserID,Q4,Q13,Q21,Q28) %>% 
  mutate(Q13=str_replace(Q13," (?<!\\w)\\(federal, state, or municipal\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(e\\.g\\. Imazon, The Nature Conservancy, etc\\.\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(dissertations, non\\-government data archives, colleagues, etc\\.\\)(?!\\w)","")) %>% 
  separate_rows(Q13,sep=",") %>% 
  pivot_longer(cols=c(Q21,Q28),names_to = "activity",values_to="time") %>% 
  mutate(activity = case_when(activity == "Q21" ~ "Time spent searching for data",
                              activity == "Q28" ~ "Time spent formatting data")) %>% 
  separate_rows(time,sep=",") %>% 
  mutate(time = case_when(str_detect(Q13,"never") ~ "Never found",
                          time == "<1 hour" ~ "< 1 hr",
                          str_detect(time,"1\\-4|4\\-8") ~ "1-8 hrs",
                          time == "8-24 hours" ~ "8-24 hrs",
                          str_detect(time,"1 day\\-") ~ "24-168 hrs",
                          time == "> 1 week" ~ "> 168 hrs",
                          str_detect(time, "never") ~ "Never usable",
                          str_detect(time, "already") ~ "None"),
         activity = case_when(time == "Never found" ~ "Time spent searching for data",
                              TRUE ~ activity)) %>%
  filter(!is.na(time)) %>% 
  distinct(UserID,Q4,activity,time) %>% 
  group_by(activity) %>% 
  count(time)

time.tots$time <- fct_relevel(time.tots$time, c("None","< 1 hr", "1-8 hrs",
                                                "8-24 hrs", "24-168 hrs",
                                                "> 168 hrs", "Never found",
                                                "Never usable"))

time.tots$activity <- fct_relevel(time.tots$activity,c("Time spent searching for data",
                                                      "Time spent formatting data"))

ggplot(time.tots,aes(x=time,y=n))+
  geom_bar(stat="identity")+
  facet_wrap(~activity,scales="free_x")+
  labs(x="",y="Number of datasets")+
  theme(legend.position="bottom", panel.border=element_rect(fill=NA),axis.line = element_line(color = "black"),
        panel.background=element_blank(),axis.text.x = element_text(angle = 60,hjust=1))

ggsave("./Figures/Fig7.png", height=4,width=8,dpi=600)

q13 <- dat %>% 
  select(UserID,Q4,Q13,Q21) %>% 
  mutate(Q13=str_replace(Q13," (?<!\\w)\\(federal, state, or municipal\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(e\\.g\\. Imazon, The Nature Conservancy, etc\\.\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(dissertations, non\\-government data archives, colleagues, etc\\.\\)(?!\\w)","")) %>% 
  separate_rows(Q13,sep=",") %>% 
  pivot_longer(cols=c(Q21),names_to = "activity",values_to="time") %>% 
  mutate(time = case_when(time == "<1 hour" ~ "< 1 hr",
                          str_detect(time,"1\\-4|4\\-8") ~ "1-8 hrs",
                          time == "8-24 hours" ~ "8-24 hrs",
                          str_detect(time,"1 day\\-") ~ "24-168 hrs",
                          time == "> 1 week" ~ "> 168 hrs",
                          str_detect(time, "never") ~ "Never usable",
                          str_detect(time, "already") ~ "None",
                          TRUE ~ time)) %>% 
  separate_rows(time,sep=",") %>% 
  distinct(UserID,Q4,time) %>% 
  filter(!is.na(time)) %>% 
  count(time)

## Unfound outcomes
unfound <- dat %>% 
  select(UserID,Q13:Q18,Q29:Q32) %>% 
  filter(str_detect(Q13,"never")) %>% 
  pivot_longer(cols=-c(UserID,Q13,Q18,Q32),names_to="Questions",values_to="Outcome") %>% 
  count(Outcome) %>% 
  filter(!is.na(Outcome))

## Infrastructure types for unfound data
unfound.infra <- dat %>% 
  select(UserID,Q4,Q13) %>% 
  filter(str_detect(Q13,"never")) #%>% 
  count(Q4) 

## Types of data that could not be found
unfound.types <- dat %>% 
  select(Q4,Q13,Q5,Q6) %>% 
  filter(str_detect(Q13,"never")) %>% 
  mutate(Q5=str_replace(Q5,"(?<!\\w)\\(e\\.g\\. paved vs dirt road, reservoir vs run\\-of\\-the river\\)(?!\\w)",""),
         Q5=str_replace(Q5,"(?<!\\w)\\(e\\.g\\. current, operational\\)(?!\\w)",""),
         Q5=str_replace(Q5,"(?<!\\w)\\(e.g. capacity, voltage, bandwidth, etc\\)",""),
         Q5=case_when(Q5=="Other" & Q6%in%tech.info ~ "Technical information ",
                      Q5=="Other" & Q6%in%lic.info ~ "Licensing dates",
                      Q5=="Other" & Q6=="EIA/RIMA" ~ "Environmental Impact Assessment (EIA) documents",
                      TRUE ~ Q5)) %>% 
  separate_rows(Q5,sep=",") %>% 
  mutate(Q5 = case_when(Q5 == "Precise location" ~ "Point location",
                        Q5 == "Environmental Impact Assessment (EIA) documents" ~ 
                          "EIA documents",
                        Q5 == "Geographic coordinates for the full extent of the project (e.g. exact location of the entire road)" ~
                          "Spatial extent",
                        Q5 == "Owner or operator or construction company" ~
                          "Owner",
                        Q5 == "Budget information" ~ "Budget",
                        TRUE ~ Q5)) %>% 
  group_by(Q5) %>% 
  count(Q5) %>% 
  filter(!is.na(Q5))

ggplot(unfound.types,aes(x=n,y=fct_reorder(Q5,n,.desc=TRUE)))+geom_bar(stat="identity")
  
## What happened projects where data were not found or were unusable
unusable <- dat %>% 
  select(UserID,Q4,Q13,Q15,Q16,Q17,Q28,Q29,Q30,Q31) %>% 
  filter(str_detect(Q28,"never")|str_detect(Q13,"never")) %>% 
  mutate(activity = case_when(str_detect(Q28,"never") ~ "Formatting",
                              str_detect(Q13,"never") ~ "Searching"),
         Q29 = case_when(is.na(Q29)~Q15,
                         TRUE ~ Q29),
         Q30 = case_when(is.na(Q30)~Q16,
                         TRUE ~ Q30),
         Q31 = case_when(is.na(Q31)~Q17,
                         TRUE ~ Q31)) %>% 
  select(-c(Q15:Q17)) %>% 
  pivot_longer(cols=c(Q29:Q31),names_to="Qs",values_to="outcomes") %>% 
  group_by(activity) %>% 
  count(outcomes)

## Reasons why data was unusable
unusable.reasons <- dat %>% 
  select(UserID,Q4,Q21,Q22,Q23,Q24,Q25,Q26,Q27,Q28) %>% 
  filter(str_detect(Q28,"never")) %>% 
  mutate(Q22 = case_when(str_detect(Q22, "Yes") ~ "Yes",
                         str_detect(Q22, "not") ~ "No"),
         Q24 = case_when(str_detect(Q24, "Yes") ~ "Yes",
                         str_detect(Q24, "not") ~ "No"),
         Q27 = case_when(str_detect(Q27, "Yes") ~ "Yes",
                         str_detect(Q27, "No") ~ "No"),
         Q23 = case_when(str_detect(Q23,"included some") ~ "Low",
                         str_detect(Q23, "did not require") ~ "NA",
                         str_detect(Q23, "had all the information") ~ "High"),
         Q25 = case_when(str_detect(Q25,"moderately")~"Moderate",
                         str_detect(Q25,"Yes")~"High",
                         str_detect(Q25,"did not") ~ "NA"),
         Q26 = case_when(str_detect(Q26,"moderately") ~ "Moderate",
                         str_detect(Q26, "I don't know") ~ "Unsure",
                         str_detect(Q26, "innacurate") ~ "Low",
                         str_detect(Q26, "Yes") ~ "High")) %>%
  pivot_longer(cols=c(Q22,Q23,Q24,Q25,Q26,Q27),names_to="Standard",values_to="YN") %>% 
  mutate(Standard = case_when(Standard == "Q22" ~ "Machine readable",
                              Standard == "Q24" ~ "Current",
                              Standard == "Q27" ~ "Complete",
                              Standard == "Q23" ~ "Metadata quality",
                              Standard == "Q25" ~ "Spatial data accuracy",
                              Standard == "Q26" ~ "Non-spatial data accuracy"),
         YN = ifelse(YN=="NA",NA,YN)) %>% 
  group_by(Standard) %>% 
  count(YN) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  filter(!is.na(YN))

## Infrastructure types for unusable data
unusable.types <- dat %>% 
  select(Q4,Q13,Q28) %>% 
  filter(str_detect(Q28, "never")) %>%
  mutate(Q13=str_replace(Q13," (?<!\\w)\\(federal, state, or municipal\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(e\\.g\\. Imazon, The Nature Conservancy, etc\\.\\)(?!\\w)",""),
         Q13=str_replace(Q13," (?<!\\w)\\(dissertations, non\\-government data archives, colleagues, etc\\.\\)(?!\\w)",""),
         Gov=case_when(str_detect(Q13,"Brazilian")~1,
                       TRUE ~ 0),
         Academic=case_when(str_detect(Q13,"Academic")~1,
                            TRUE ~ 0),
         NGO = case_when(str_detect(Q13,"Non\\-government")~1,
                         TRUE ~ 0),
         Other = case_when(str_detect(Q13,"Other")~1,
                           TRUE ~ 0),
         sum = Gov + Academic + NGO + Other,
         source = case_when(Gov == 1 & sum == 1 ~ "Government",
                            Gov == 0 & sum >= 1 ~ "Non-government",
                            Gov == 1 & sum > 1 ~ "Both")) %>% 
  group_by(source) %>% 
  count(Q4)

unusable.qual <- unusable.reasons %>% filter(str_detect(Standard,"accuracy"))
qual <- ggplot(dat.qual,aes(x=perc,y=source,fill=accuracy))+
  geom_bar(stat="identity")+
  geom_text(aes(label=paste0(round(perc,0),"%")),color="white",position = position_stack(vjust = 0.5))+
  facet_wrap(~spat,ncol=1)+
  scale_fill_manual(values=c("#fdae61","#74add1","#4575b4","#313695"),
                    labels = c("Unsure","Low","Moderate","High"),name = "Response")+
  theme(legend.position="bottom", panel.border=element_rect(fill=NA),axis.line = element_line(color = "black"),
        panel.background=element_blank())+
  labs(y="",x="Frequency (%)")+
  ggtitle("(A) Data accuracy")

ti <- ggplot(task.ind,aes(x=perc,y=source,fill=YN))+
  geom_bar(stat="identity")+
  geom_text(aes(label=paste0(round(perc,0),"%")),color="white",position=position_stack(vjust=0.5))+
  facet_wrap(~Standard,ncol=1)+
  scale_fill_manual(values=c("#fdae61","#a50026","#313695"),
                    labels = c("N/A","No/Low","Yes/High"),name = "Response")+
  theme(legend.position="bottom", panel.border=element_rect(fill=NA),axis.line = element_line(color = "black"),
        panel.background=element_blank())+
  labs(y="",x="Frequency (%)")+
  ggtitle("(B) Task-independent standards")

qual/ti

time.tots %>% filter(activity == "Time spent searching for data") %>% 
  ggplot(aes(x=time,y=n))+
  geom_bar(stat="identity")

time.tots %>% filter(activity == "Time spent formatting data") %>% 
  ggplot(aes(x=time,y=n))+
  geom_bar(stat="identity")

