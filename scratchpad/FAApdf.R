library(readtext)

orig <- readtext('2017 FAA Congressional Log.txt')

q <- unlist(strsplit(orig$text, "Control Number"))
data <- data.frame(q)
data$nothing <- NA
data <- data[-1,]
data %<>%
  separate(q, into= c("Control Number", "FROM"), sep= "Writer\\(s\\):") %>% 
  separate(FROM, into= c("FROM", "DocumentDate"), sep= "Document Date:") %>% 
  separate(DocumentDate, into= c("DocumentDate", "AssignedTo"), sep="Assigned To: ") %>% 
  separate(`AssignedTo`, into= c("AssignedTo", "DueDate"), sep="Due Date: ") %>% 
  separate(DueDate, into= c("DueDate", "SignedDate"), sep= "Signed Date: ") %>% 
  separate(SignedDate, into= c("SignedDate", "Analyst"), sep= "Analyst: ") %>% 
  separate(Analyst, into= c("Analyst", "Writer/Editor"), sep= "Writer/Editor: ") %>% 
  separate(`Writer/Editor`, into= c("Writer/Editor", "Significant"), sep= "Significant:") %>% 
  separate(`Significant`, into= c("Significant", "Folder Template"), sep= "Folder Template: ") %>% 
  separate(`Folder Template`, into= c("Folder Template", "Addressed To"), sep= "Addressed To:") %>% 
  separate(`Addressed To`, into= c("Addressed To", "On Behalf Of"), sep= "On Behalf Of:") %>% 
  separate(`On Behalf Of`, into= c("On Behalf Of", "Cross Reference"), sep= "Cross Reference:") %>% 
  separate(`Cross Reference`, into= c("Cross Reference", "White House Reference"), sep= "White House Reference:") %>% 
  separate(`White House Reference`, into= c("White House Reference", "Relate Control"), sep= "Related Control:") 









           





