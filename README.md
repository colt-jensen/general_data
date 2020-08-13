# general_code

library(readxl)
read_excel("Miss-Data.xls")
head(Miss_Data)
library(forcats)
library(dplyr)
count(Miss_Data, Miss_Data$City)
City_2 <- fct_collapse(Miss_Data$City, Biloxi = c("Biloxi", "Bilxoi"), Pascagoula = c("Pascagola", "Pascagoula"))
fct_count(City_2)
Miss_Data$City <- City_2
Miss_Data %>%
 count(City)
 
lm(PSM ~ PSNPS + Age, data=Miss_Data) %>% 
 summary() 

# Comments start with a pound sign/hashtag

Miss_Data$PSM <- as.factor(Miss_Data$PSM)

ggplot(data = Miss_Data) +
 geom_point(aes(x = PSM, y = Age))
 




str(X6_17_Colt_Capstone_Miss_data_set)
count(X6_17_Colt_Capstone_Miss_data_set$City)
install.packages("plot_ly")
glimpse(Miss_Data_Complete)
head(Miss_Data_Complete)
X6_17_Colt_Capstone_Miss_data_set <- read_excel("Downloads/6 17 Colt Capstone Miss data set.xls")
library(readxl)
X6_17_Colt_Capstone_Miss_data_set <- read_excel("Downloads/6 17 Colt Capstone Miss data set.xls")
View(X6_17_Colt_Capstone_Miss_data_set)
head(X6_17_Colt_Capstone_Miss_data_set)
glimpse(X6_17_Colt_Capstone_Miss_data_set)
summary(X6_17_Colt_Capstone_Miss_data_set)
pairs(X6_17_Colt_Capstone_Miss_data_set)
library(dplyr)
glimpse(ggplot(Price ~ Food, data = nyc))
glimpse(X6_17_Colt_Capstone_Miss_data_set)
lm(PSM ~ PSNPS + Age + Gender + Age:Gender, data = X6_17_Colt_Capstone_Miss_data_set)
fit <- lm(PSM ~ PSNPS + Age + Gender + Age:Gender, data = X6_17_Colt_Capstone_Miss_data_set)
install.packages(gvlma)
install.packages("gvlma")
gvmodel <- gvlma(fit)
summary(gvmodel)
summary.lm(PSM ~ PSNPS + Age + Gender + Age:Gender, data = X6_17_Colt_Capstone_Miss_data_set)
model=lm(PSM ~ PSNPS + Age + Gender + Age:Gender, data = X6_17_Colt_Capstone_Miss_data_set)
summary(model)
library(stringr)
library(tidyr)
install.packages("tidyr")
install.packages("tidyr")
ls
ls()
city(mydata)
City("X6_17_Colt_Capstone_Miss_data_set")
levels(X6_17_Colt_Capstone_Miss_data_set$City)
categories <- unqiue(X6_17_Colt_Capstone_Miss_data_set$City)
categories <- unqiue(X6_17_Colt_Capstone_Miss_data_set$City) numberOfCategories <- length(categories)
table(X6_17_Colt_Capstone_Miss_data_set$City)
sort(table(X6_17_Colt_Capstone_Miss_data_set$City), decreasing = TRUE)
strtrim(X6_17_Colt_Capstone_Miss_data_set$City, "")
levels(X6_17_Colt_Capstone_Miss_data_set$City)
levels(City)
nlevels(X6_17_Colt_Capstone_Miss_data_set$City)
categories <- unique(X6_17_Colt_Capstone_Miss_data_set$City)
numberOfCategories <- length(categories)
print(numberOfCategories)
X6_17_Colt_Capstone_Miss_data_set %>%
distinct(City, .keep_all = TRUE)
strwidth(X6_17_Colt_Capstone_Miss_data_set$City)
library(dplyr)
count(X6_17_Colt_Capstone_Miss_data_set$City)
count(X6_17_Colt_Capstone_Miss_data_set, City)
nchar(X6_17_Colt_Capstone_Miss_data_set$City, keepNA = FALSE)
nchar(X6_17_Colt_Capstone_Miss_data_set$City, type = "chars", allowNA = FALSE, keepNA = FALSE)
duplicated(X6_17_Colt_Capstone_Miss_data_set$City)
X6_17_Colt_Capstone_Miss_data_set %>%
count(City)
filter(n>1)
library(tidyr)
transmute(X6_17_Colt_Capstone_Miss_data_set$City, Pascagoula <- c(Pascagoula, Pascagola))
X6_17_Colt_Capstone_Miss_data_set$City <- c(Pascagoula, Pascagola)
c(Pascagoula, Pascagola)
X6_17_Colt_Capstone_Miss_data_set$City <- c(X6_17_Colt_Capstone_Miss_data_set$City$Pascagoula, X6_17_Colt_Capstone_Miss_data_set$City$Pascagola)
X6_17_Colt_Capstone_Miss_data_set$City <- combine(X6_17_Colt_Capstone_Miss_data_set$City, list(name "Pascagoula" = c("Pascagoula","Pascagola"))
X6_17_Colt_Capstone_Miss_data_set$City <- combine(X6_17_Colt_Capstone_Miss_data_set$City, list(name "Pascagoula" = c("Pascagoula","Pascagola")))
list(X6_17_Colt_Capstone_Miss_data_set$City)
X6_17_Colt_Capstone_Miss_data_set$City <- status
table(X6_17_Colt_Capstone_Miss_data_set$City)
library(assertive)
assert_is_character(X6_17_Colt_Capstone_Miss_data_set$City)
c(Bilxoi, Biloxi, X6_17_Colt_Capstone_Miss_data_set$City)
X6_17_Colt_Capstone_Miss_data_set$City <- c(Bilxoi, Biloxi)
install.packages(recordlinkage)
install.packages("RecordLinkage")
jarowinkler('Bilxoi', "Biloxi")
install.packages("RSQLite")
jarowinkler('Bilxoi', "Biloxi"))
jarowinkler('Bilxoi', "Biloxi")
stringdist('Biloxi', 'Bilxoi', method = 'jw')
library(stringdist)
stringdist('Biloxi', 'Bilxoi', method = 'jw')
stringdist('Biloxi', 'Bilxoi', method = 'lv')
table(X6_17_Colt_Capstone_Miss_data_set,City)
table(X6_17_Colt_Capstone_Miss_data_set$City)
stringdist('Pascagola', 'Pascagoula', method = 'lv')
adist('Pascagola', "Pascagoula")
library(readxl)
View(X6_17_Colt_Capstone_Miss_data_set)
Miss_Data <- read_xls(X6_17_Colt_Capstone_Miss_data_set)
class(X6_17_Colt_Capstone_Miss_data_set$City)
levels(X6_17_Colt_Capstone_Miss_data_set$City)
X6_17_Colt_Capstone_Miss_data_set$City <- trimws(X6_17_Colt_Capstone_Miss_data_set$City)
Biloxi <- c("Biloxi", "Bilxoi")
table(X6_17_Colt_Capstone_Miss_data_set$City)
amatch
table(X6_17_Colt_Capstone_Miss_data_set$City)
adist('Biloxi','Bilxoi')
stringdist('Biloxi','Bilxoi', method = "lv")
stringdist('Pascagola','Pascagoula ', method = "lv")
stringdist(tolower("Biloxi"), "biloxi")
Biloxi_one <- c("Biloxi", "Bilxoi")
library(forcats)
X6_17_Colt_Capstone_Miss_data_set %>%
count(City)
X6_17_Colt_Capstone_Miss_data_set %>%
mutate(Biloxi_Collapsed= fct_collapse(City, Biloxi = Biloxi_one)) %>% count(Biloxi_Collapsed)
Pascagoula_one <- c("Pascagola", "Pascagoula")
X6_17_Colt_Capstone_Miss_data_set %>%
+     mutate(Cities_Collapsed = fct_collapse(City, Biloxi = Biloxi_one), fct_collapse(City, Pascagoula = Pascagoula_one)) %>% count(Cities_Collapsed)
X6_17_Colt_Capstone_Miss_data_set %>%
+     mutate(Pascagoula_Collapsed= fct_collapse(City, Pascagoula = Pascagoula_one)) %>% count(Pascagoula_one)
X6_17_Colt_Capstone_Miss_data_set %>%
+     mutate(Biloxi_Collapsed= fct_collapse(City, Biloxi = Biloxi_one)) %>% count(Biloxi_Collapsed)
X6_17_Colt_Capstone_Miss_data_set %>%
count(City)
X6_17_Colt_Capstone_Miss_data_set %>%
mutate(Biloxi_Collapsed= fct_collapse(City, Biloxi = Biloxi_one)) %>% count(Biloxi_Collapsed)
X6_17_Colt_Capstone_Miss_data_set %>%
+     count(City)
X6_17_Colt_Capstone_Miss_data_set %>%
count(City)
X6_17_Colt_Capstone_Miss_data_set %>%
+     mutate(Cities_collapsed= fct_collapse(City, Biloxi = Biloxi_one), fct_collapse(City, Pascagoula = Pascagoula_one)) %>% count(Biloxi_Collapsed)
X6_17_Colt_Capstone_Miss_data_set %>%
count(City)
X6_17_Colt_Capstone_Miss_data_set %>%
mutate(Pascagoula_collapsed= fct_collapse(City, Pascagoula = Pascagoula_one)) %>% count(Pascagoula_collapsed)
X6_17_Colt_Capstone_Miss_data_set %>%
count(City)
X6_17_Colt_Capstone_Miss_data_set %>%
mutate(Cities_collapsed= fct_collapse(City, Pascagoula = Pascagoula_one) + fct_collapse(City, Biloxi = Biloxi_one)) %>% count(Cities_collapsed)
recode(X6_17_Colt_Capstone_Miss_data_set$City, 'Bilxoi = Biloxi')
recode(X6_17_Colt_Capstone_Miss_data_set$City, 'Bilxoi = Biloxi', as.factor=TRUE)
recode(X6_17_Colt_Capstone_Miss_data_set$City, 'Bilxoi = Biloxi', as.factor=TRUE, as.numeric = FALSE)
recode(X6_17_Colt_Capstone_Miss_data_set$City, 'Bilxoi = Biloxi', as.factor=TRUE)
rlang::last_error()
na_if(Bilxoi)
X6_17_Colt_Capstone_Miss_data_set %>%
select(City) %>%
mutate(City = 'Biloxi = Bilxoi')
library(readxl)
Miss_Data <- read_excel("Downloads/Miss-Data.xls")
View(Miss_Data)
count(Miss_Data$City)
Miss_Data %>%
count(City)
Miss_Data %>%
select_if(City, Bilxoi) %>%
mutate('Bilxoi=Biloxi')
Miss_Data %>%
+     select_if(City, "Bilxoi") %>%
+     mutate('Bilxoi=Biloxi')
Miss_Data %>%
select(City) %>%
mutate(Bilxoi, 'Bilxoi = Biloxi')
Miss_Data %>%
select(City) %>%
mutate(if_else(Biloxi, 'Bilxoi = Biloxi'))
Miss_data %>%
select(City) %>%
replace(Bilxoi, "Bilxoi = Biloxi")
library(readxl)
Miss_Data <- read_excel("Downloads/Miss-Data.xls")
View(Miss_Data)
library(dpylr)
library(dplyr)
library(tidyr)
Miss_Data %>%
count(City)
library(forcats)
Miss_Data %>%
fct_collapse(Miss_Data$City, Biloxi = c("Bilxoi", "Biloxi"), Pascagoula = c("Pascagola","Pascagoula"))
Miss_Data <- fct_collapse(Miss_Data$City, Biloxi = c("Bilxoi", "Biloxi"), Pascagoula = c("Pascagola","Pascagoula"))
count(Miss_Data$City)
Miss_Data %>%
count(V1)
count(Miss_Data)
Miss_Data %>%
count(Miss_Data)
fct_count(Miss_Data)
height <- c(25,18,40,45,35)
height(5)
height[length(height)]
