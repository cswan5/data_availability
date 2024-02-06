library(tidyverse)

## Comparison between survey and literature review
lit <- read.csv("./Data/lit_review_summary.csv")

## Calculate the number of times each attribute was used and the proportion each
## attribute was used per papers
lit.prop <- lit %>% 
  count(attributes) %>% 
  mutate(Proportion = n/62,
         Source = "Uses per paper in literature review") %>% # divided by the number of papers in the summary
  select(-c(n))

## Rename columns to match
names(lit.prop) <- c("Attribute","Proportion","Source")

## Ranked attribute data from survey
fw <- read.csv("./Data/framework.csv") %>% 
  select(3:16) %>% 
  ## Change the top five ranked attributes into 1 and all others into 0
  mutate(across(Name:Tech_info, ~ case_when(. <= 5 ~ 1,
                                            . > 5 ~ 0)))

## Total the columns to get the number of times an attribute was ranked as one 
## of the top five
fw.summ <- colSums(fw)

## Proportion of times an attribute was ranked as one of the top 5 in survey
fw.prop <- data.frame(Attribute = names(fw.summ),
                      Proportion = fw.summ/nrow(fw),
                      Source = "Ranked top 5 in survey")

## Change the attribute names to be consistent
props <- bind_rows(lit.prop,fw.prop) %>% 
  mutate(Attribute = case_when(str_detect(Attribute,"(?i)name") ~ "Project name",
                               str_detect(Attribute,"(?i)const_date|construction date") ~ "Construction date",
                               str_detect(Attribute,"op_date|Ops_date|operation date") ~ "Operation date",
                               str_detect(Attribute,"(?i)extent") ~ "Spatial extent",
                               str_detect(Attribute,"(?i)owner") ~ "Owner",
                               str_detect(Attribute,"(?i)budget") ~ "Budget",
                               str_detect(Attribute,"loc|(?i)Location") ~ "Project location",
                               str_detect(Attribute,"(?i)type") ~ "Project type",
                               str_detect(Attribute,"(?i)EIA") ~ "EIA documents",
                               str_detect(Attribute,"(?i)licens.*") ~ "Licensing dates",
                               str_detect(Attribute,"op_cost|Ops_cost|operating cost") ~ "Operating costs",
                               str_detect(Attribute,"(?i)status") ~ "Project status",
                               str_detect(Attribute,"(?i)funding") ~ "Funding source",
                               str_detect(Attribute,"(?i)tech.*") ~ "Technical information")) %>% 
  filter(Attribute != "other")

## Plot the survey vs literature review results
ggplot(props, aes(x=Proportion,y=reorder(Attribute,-Proportion),fill=Source))+
  geom_bar(stat="identity",position = "dodge")+
  geom_vline(aes(xintercept=0.4),linetype="dashed")+
  scale_fill_manual(values=c("#313695","#fdae61"))+
  labs(x="Proportion",y="Attribute")+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(color = "black"))

ggsave("./Figures/Fig4.png",height=4,width=6)

## Get rankings in survey
ranks <- read.csv("./Data/framework_clean.csv")
rankings <- read.csv("./Data/framework.csv") %>% 
  summarise(across(Name:Tech_info,mean)) %>% 
  pivot_longer(cols=Name:Tech_info,names_to="Attribute",values_to = "Ranking") %>% 
  mutate(Attribute = case_when(Attribute == "Name" ~ "Project name",
                               Attribute == "Const_date" ~ "Construction date",
                               Attribute == "Ops_date" ~ "Operation date",
                               Attribute == "Extent" ~ "Spatial extent",
                               Attribute == "Location" ~ "Project location",
                               Attribute == "Type" ~ "Poject type",
                               Attribute == "EIA" ~ "EIA documents",
                               Attribute == "License_dates" ~ "Licensing dates",
                               Attribute == "Ops_cost" ~ "Operating costs",
                               Attribute == "Status" ~ "Project status",
                               Attribute == "Funding" ~ "Funding source",
                               Attribute == "Tech_info" ~ "Technical infomration",
                               TRUE ~ Attribute))

ggplot(rankings,aes(x=reorder(Attribute,Ranking),y=Ranking))+
  geom_bar(stat="identity")+
  geom_hline(aes(yintercept=7.5))+
  labs(x="Attribute",y="Average ranking")+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle=90))

