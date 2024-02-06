library(tidyverse)
library(patchwork)

## Read in the literature review summary file
lit <- read.csv("./Data/literature_review.csv")            

## Count the number of different attributes used 
atts <- lit %>% count(Attributes) %>% 
  mutate(Attributes=fct_reorder(Attributes,n,.desc=TRUE))

## Count the types of infrastructure data used for the studies
infra.used <- lit %>% distinct(Citation,Overarching_infrastructure,Dataset_used,Dataset_infrastructure_type) %>% 
  count(Dataset_infrastructure_type) %>% 
  mutate(Dataset_infrastructure_type=fct_reorder(Dataset_infrastructure_type,n,.desc=TRUE))

discs <- lit %>% distinct(Citation,Thematic_focus) %>% 
  count(Thematic_focus) %>% 
  mutate(Thematic_focus=fct_reorder(Thematic_focus,n,.desc=TRUE))

fig1a <- ggplot(discs,aes(x=n,y=Thematic_focus))+
  geom_bar(stat="identity") +
  theme(panel.background=element_blank(),panel.border = element_rect(fill=NA))+
  labs(x="Number of studies",y="")+
  ggtitle("(A) Research themes")

fig1b <- ggplot(infra.used,aes(x=n,y=Dataset_infrastructure_type))+
  geom_bar(stat="identity")+
  theme(panel.background=element_blank(),panel.border = element_rect(fill=NA))+
  labs(x="Number of uses",y="")+
  ggtitle("(B) Infrastructure types")

fig1c <- ggplot(atts,aes(x=n,y=Attributes))+
  geom_bar(stat="identity")+
  theme(panel.background=element_blank(),panel.border = element_rect(fill=NA))+
  labs(x="Number of uses",y="")+
  ggtitle("(C) Attributes used")

fig1a/(fig1b+fig1c)

