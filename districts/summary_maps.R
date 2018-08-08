
# This is a scratchpad for drafting maps 

library(ggplot2)
library(dplyr)
library(maps)
library(fiftystater)
library(mapproj)


states <- read.csv("districts/states.csv") 
states$state %<>% tolower() 
states$pop2010 <- gsub(",","",states$pop2010)
states$pop2010 %<>% as.numeric()
states$state
df %<>% left_join(states)
df$pop2010


df %>% 
  group_by(state) %>% tally() %>%
# map_id creates the aesthetic mapping to the state name column
ggplot() + 
  # map points to the fifty_states shape data
  geom_map(aes(map_id = state, fill = n), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Letters from Members of Congress")) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank())

df %>% 
  filter(chamber == "Senate") %>% group_by(state, pop2010) %>% tally() %>%
  mutate(Per_Capita = n/pop2010) %>% 
  # map_id creates the aesthetic mapping to the state name column in your data
  ggplot() + 
  # map points to the fifty_states shape data
  geom_map(aes(map_id = state, fill = Per_Capita), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Letters from Members of Congress")) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank())


df %<>% group_by(bioname, year) %>% mutate(permemberyear = n())
popMod <- lm(permemberyear ~ pop2010 + position + partystatus, data = df %>% filter(Type2 == "Constituent Service"))
summary(popMod)

# by year 
log.year <- group_by(data, state, year) %>% count()

ggplot(log.subject, aes(map_id = state)) + 
  facet_wrap( ~ year) +
  # map points to the fifty_states shape data
  geom_map(aes(fill = n), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Letters from Members of Congress to the", agency, years)) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank())

# by type
log.type <- group_by(data, state, TYPE, year) %>% count()
log.type %<>% filter(!is.na(TYPE))

ggplot(log.type, aes(map_id = state)) + 
  facet_grid(TYPE ~ year) +
  # map points to the fifty_states shape data
  geom_map(aes(fill = n), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Letters from Members of Congress to the", agency, years, "by Type")) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank())

# by party
log.party <- group_by(data, state, party, year) %>% count()
log.party %<>% filter(party %in% c("DEM", "GOP", "IND")) %>%
  filter(!is.na(year))

ggplot(log.party, aes(map_id = state)) + 
  facet_grid(party ~ year) +
  # map points to the fifty_states shape data
  geom_map(aes(fill = n), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Letters from Members of Congress to the", agency, years)) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank())


# by subject and year 
log.subject <- group_by(data, state, SUBJECT, year) %>% count()
log.subject %<>% filter(SUBJECT %in% major.subjects) 

ggplot(log.subject, aes(map_id = state)) + 
  facet_grid(SUBJECT ~ year) +
  # map points to the fifty_states shape data
  geom_map(aes(fill = n), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Letters from Members of Congress to the", agency, years, "on Select Subjects")) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank())

# by subject and party 
log.subject <- group_by(data, state, SUBJECT, party) %>% count()
log.subject %<>% filter(SUBJECT %in% major.subjects)
log.subject %<>% filter(!is.na(state))
log.subject %<>% filter(!is.na(party))

ggplot(log.subject, aes(map_id = state)) + 
  facet_grid(SUBJECT ~ party) +
  # map points to the fifty_states shape data
  geom_map(aes(fill = n), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Letters from Members of Congress to the", agency, years, "on Select Subjects")) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank())

# by type and party 
log <- group_by(data, state, TYPE, party) %>% count()
log %<>% filter(!is.na(state))
log %<>% filter(!is.na(party))


ggplot(log, aes(map_id = state)) + 
  facet_grid(TYPE ~ party) +
  # map points to the fifty_states shape data
  geom_map(aes(fill = n), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "", title = paste("Letters from Members of Congress to the", agency, years, "by Type")) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.background = element_blank())

